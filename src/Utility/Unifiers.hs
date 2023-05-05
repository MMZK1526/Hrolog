{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal helper functions for unification.
module Utility.Unifiers where

import           Control.Lens hiding (ix)
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Tuple
import           GHC.Stack

import           Internal.Program
import           Utility.Exception
import           Utility.Graph
import           Utility.UnionFind

-- | The state that keeps track of the variables that have been encountered as
-- well as a union-find data structure recording the equivalences between
-- variables.
data UnifyState a = UnifyState { _usVarMap :: Map a Int
                               , _usUF     :: UnionFind (FunctionTerm' a)
                               , _usUseMap :: IntMap (Set a) }
  deriving Show
$(makeLenses ''UnifyState)

-- | The initial unification state.
mkUnifyState :: Ord a => UnifyState a
mkUnifyState = UnifyState M.empty mkUnionFind IM.empty
{-# INLINE mkUnifyState #-}

-- | Add a new variable to the unification state. If the variable is already
-- present, do nothing.
--
-- Return the index of the variable in the unification state.
addVar :: HasCallStack => Ord a => Monad m => a -> StateT (UnifyState a) m Int
addVar var = do
  varMap <- use usVarMap
  case varMap M.!? var of
    Just ix -> pure ix
    Nothing -> do
      ix <- use $ usUF . ufSize -- Get the next index
      usVarMap . at var ?= ix -- Add the variable to the var map
      -- Add the variable to the union-find, this also increments the size.
      usUF %= ufAdd
      pure ix
{-# INLINE addVar #-}

-- | Unify two terms under a given @UnifyState@ context. Returns 'Nothing' if
-- the terms cannot be unified.
unifyTermS :: HasCallStack => Ord a => Monad m
           => Term' a -> Term' a -> StateT (UnifyState a) (MaybeT m) ()
unifyTermS t t' = do
  unifyTermS' t t'
  useMap <- use usUseMap >>= mapM (mapM addVar . S.toList)
  uf     <- use usUF
  let graph                = Graph . fmap IS.toList
                           $ fst (IM.foldlWithKey' worker (IM.empty, uf) useMap)
      worker (g, uf') ix s = do
        let (rep, uf'')  = ufFind ix uf'
        let (pts, uf''') = ufFinds s uf''
        (IM.insertWith IS.union rep (IS.fromList pts) g, uf''')
  guard (not $ hasCycle graph)

-- | Similar to @unifyTerm@, but do not eliminate recursive usages of variables.
unifyTermS' :: HasCallStack => Ord a => Monad m
            => Term' a -> Term' a -> StateT (UnifyState a) (MaybeT m) ()
unifyTermS' (F f ts) (F f' ts')
  | f /= f'   = mzero
  | otherwise = mapM_ (uncurry unifyTermS') (zip ts ts')
unifyTermS' (VariableTerm v) t                 = do
  void $ addVar v -- Make sure the variable is in the var map
  case t of
    -- If the term is a function, we can just set the value of the variable
    -- to the function, as long as there are no recursive usages of the variable
    -- itself. We store the set of variables that are used in the function in
    -- the @UnifyState@.
    F f ts -> do
      varMap <- use usVarMap
      uf     <- use usUF
      let errMsg
            = concat ["unifyTermS': The element ", show ix," does not exist."]
          ix     = fromMaybe (throw errMsg) $ varMap M.!? v
      let (mVal, uf') = ufGet ix uf
      case mVal of
        Just ft -> unifyTermS' t (FunctionTerm ft)
        Nothing -> do
          usUF .= ufSet (Just $ FTerm f ts) ix uf'
          let usedVars = S.fromList $ mapMaybe (\case
                VariableTerm v' -> Just v'
                _               -> Nothing) ts
          usUseMap . at ix %= Just . S.union usedVars . fromMaybe S.empty
    -- If the term is another variable, we can just union the two variables,
    -- subjecting to the constraint that the two variables are not already
    -- set to different constants.
    VariableTerm v' -> do
      void $ addVar v' -- Make sure the other variable is in the var map
      varMap <- use usVarMap
      uf     <- use usUF
      let errMsg        = "unifyTermS': Accessing elements not in the var map."
      let ix            = fromMaybe (throw errMsg) $ varMap M.!? v
      let ix'           = fromMaybe (throw errMsg) $ varMap M.!? v'
      let (mVal, uf')   = ufGet ix uf
      let (mVal', uf'') = ufGet ix' uf'
      case (mVal, mVal') of
        -- Unify the values.
        (Just f, Just f')  -> do
          unifyTermS' (FunctionTerm f) (FunctionTerm f')
          usUF .= uf''
        -- If only one of the variables is set, union with the other variable.
        _usUF              -> usUF .= ufUnion ix ix' uf''
unifyTermS' t (VariableTerm v)                 = unifyTermS' (VariableTerm v) t

-- | Unify two atoms under a given @UnifyState@ context. Returns 'Nothing' if
-- the atoms cannot be unified.
unifyAtomS :: HasCallStack => Ord a => Monad m
           => Atom' a -> Atom' a -> StateT (UnifyState a) (MaybeT m) ()
unifyAtomS (Atom p ts) (Atom p' ts')
  | p /= p'   = mzero
  | otherwise = mapM_ (uncurry unifyTermS) (zip ts ts')
{-# INLINE unifyAtomS #-}

-- | Unify two atoms. Returns a substitution that, when applied, would make the
-- two atoms equal. Returns 'Nothing' if the atoms cannot be unified.
--
-- The substitution is represented as a @Map@ from variables to terms.
unifyAtom :: HasCallStack => Ord a
          => Atom' a -> Atom' a -> Maybe (Map a (Term' a))
unifyAtom a a' = case mUS of
  Nothing -> Nothing -- Unification failed
  Just us -> fst <$> do
    -- Convert the union-find indices to variables
    let ixToVarMap = IM.fromList $ map swap $ M.assocs (us ^. usVarMap)
    let uf         = us ^. usUF -- The union-find after unification
    -- For each variable in the var map, map it to the term corresponding to its
    -- representative in the union-find.
    Just . flip execState (M.empty, uf)
         . forM_ (M.assocs (us ^. usVarMap)) $ \(v, ix) -> do
      let (mVal, uf') = ufGet ix uf -- Get the value of the representative
      case mVal of
        -- If the value is Nothing, we map the variable to its representative.
        Nothing           -> do
          let (ix', uf'') = ufFind ix uf'
          let errMsg      = concat [ "unifyAtom: The element ", show ix'
                                   , " does not exist." ]
          _1 . at v
            ?= VariableTerm (fromMaybe (throw errMsg) $ ixToVarMap IM.!? ix')
          _2 .= uf'' -- Update the union-find
        -- If the value is a constant, we map the variable to it.
        Just (FTerm f ts) -> do
          _1 . at v ?= F f ts
          _2 .= uf' -- Update the union-find
  where
    mUS = runIdentity . runMaybeT $ execStateT (unifyAtomS a a') mkUnifyState

-- | Similar to @unifyTermS@, but only allows subsumption.
subsumeTermS :: HasCallStack => Ord a => Monad m
             => Term' a -> Term' a -> StateT (UnifyState a) (MaybeT m) ()
subsumeTermS t t' = do
  subsumeTermS' t t'
  useMap <- use usUseMap >>= mapM (mapM addVar . S.toList)
  uf     <- use usUF
  let graph                = Graph . fmap IS.toList
                           $ fst (IM.foldlWithKey' worker (IM.empty, uf) useMap)
      worker (g, uf') ix s = do
        let (rep, uf'')  = ufFind ix uf'
        let (pts, uf''') = ufFinds s uf''
        (IM.insertWith IS.union rep (IS.fromList pts) g, uf''')
  guard (not $ hasCycle graph)

-- | Similar to @subsumeTerm@, but do not eliminate recursive usages of
-- variables.
subsumeTermS' :: HasCallStack => Ord a => Monad m
              => Term' a -> Term' a -> StateT (UnifyState a) (MaybeT m) ()
subsumeTermS' (F _ (_ : _)) (VariableTerm _) = mzero
subsumeTermS' t t' = unifyTermS' t t'

-- | Similar to @unifyAtomS@, but only allows subsumption.
subsumeAtomS :: HasCallStack => Ord a => Monad m
             => Atom' a -> Atom' a -> StateT (UnifyState a) (MaybeT m) ()
subsumeAtomS (Atom p ts) (Atom p' ts')
  | p /= p'   = mzero
  | otherwise = mapM_ (uncurry subsumeTermS) (zip ts ts')
{-# INLINE subsumeAtomS #-}

-- | Return "True" if the first atom subsumes the second atom.
subsumes :: HasCallStack => Ord a => Atom' a -> Atom' a -> Bool
subsumes a a' = isJust . runIdentity . runMaybeT
              $ execStateT (subsumeAtomS a a') mkUnifyState

-- | Substitute a term with the given substitution map.
--
-- Need to apply the substitution until the result is fixed because the result
-- of the substitution may itself contain variables that need to be substituted.
substituteTerm :: Ord a => Map a (Term' a) -> Term' a -> Term' a
substituteTerm sub = fixPoint worker
  where
    worker t = case t of
      VariableTerm v -> fromMaybe t (M.lookup v sub)
      F f ts         -> F f (map worker ts)
{-# INLINE substituteTerm #-}

-- | Substitute an atom with the given substitution map.
substituteAtom :: Ord a => Map a (Term' a) -> Atom' a -> Atom' a
substituteAtom m (Atom p ts) = Atom p (map (substituteTerm m) ts)
{-# INLINE substituteAtom #-}

-- Apply the renaming function to all variables in the term.
renameTerm :: (a -> b) -> Term' a -> Term' b
renameTerm f (VariableTerm x) = VariableTerm $ f x
renameTerm f (F t ts)         = F t (map (renameTerm f) ts)
{-# INLINE renameTerm #-}

-- Apply the renaming function to all variables in the atom.
renameAtom :: (a -> b) -> Atom' a -> Atom' b
renameAtom f (Atom p ts) = Atom p (map (renameTerm f) ts)
{-# INLINE renameAtom #-}

-- | Apply the renaming function to all variables in the clause.
renameClause :: (a -> b) -> Clause' a -> Clause' b
renameClause f (Clause h b) = Clause (renameAtom f <$> h) (map (renameAtom f) b)
{-# INLINE renameClause #-}

-- | Apply the renaming function to all variables in the program.
renameProgram :: (a -> b) -> Program' a -> Program' b
renameProgram f p = p & clauses %~  map (renameClause f)
{-# INLINE renameProgram #-}

-- | Apply the renaming function to all variables in the query.
renamePQuery :: Ord b => (a -> b) -> PQuery' a -> PQuery' b
renamePQuery f q = q { _pqVariables  = S.map f (_pqVariables q)
                     , _pqAtoms      = map (renameAtom f) (_pqAtoms q) }
{-# INLINE renamePQuery #-}

-- | Apply an automorphism until the result is fixed.
fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x = let x' = f x in if x == x' then x else fixPoint f x'
