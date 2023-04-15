{-# LANGUAGE TemplateHaskell #-}

-- | Internal helper functions for unification.
module Utility.Unifiers where

import           Control.Lens hiding (ix)
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Functor
import qualified Data.IntMap as IM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Tuple

import           Internal.Program
import           Utility.UnionFind

-- | The state that keeps track of the variables that have been encountered as
-- well as a union-find data structure recording the equivalences between
-- variables.
data UnifyState a = UnifyState { _usVarMap :: Map a Int
                               , _usUF     :: UnionFind Constant }
  deriving Show
$(makeLenses ''UnifyState)

-- | The initial unification state.
mkUnifyState :: Ord a => UnifyState a
mkUnifyState = UnifyState M.empty mkUnionFind
{-# INLINE mkUnifyState #-}

-- | Add a new variable to the unification state. If the variable is already
-- present, do nothing.
addVar :: Ord a => Monad m => a -> StateT (UnifyState a) m ()
addVar var = do
  varMap <- use usVarMap
  case varMap M.!? var of
    Just _  -> return ()
    Nothing -> do
      ix <- use $ usUF . ufSize -- Get the next index
      usVarMap . at var ?= ix -- Add the variable to the var map
      -- Add the variable to the union-find, this also increments the size.
      usUF %= ufAdd
{-# INLINE addVar #-}

-- | Unify two terms under a given @UnifyState@ context. Returns 'Nothing' if
-- the terms cannot be unified.
unifyTermS :: Ord a => Monad m
           => Term' a -> Term' a -> StateT (UnifyState a) (MaybeT m) ()
unifyTermS (ConstantTerm c) (ConstantTerm c') = guard (c == c') $> ()
unifyTermS (VariableTerm v) t                 = do
  addVar v -- Make sure the variable is in the var map
  case t of
    -- If the term is a constant, we can just set the value of the variable
    -- to the constant, subjecting to the constraint that the variable is
    -- not already set to a different constant.
    ConstantTerm c -> do
      varMap <- use usVarMap
      uf     <- use usUF
      let ix = varMap M.! v
      let (mVal, uf') = ufGet ix uf
      case mVal of
        Nothing -> usUF .= ufSet (Just c) ix uf'
        Just c' -> guard (c == c') >> (usUF .= uf')
    -- If the term is another variable, we can just union the two variables,
    -- subjecting to the constraint that the two variables are not already
    -- set to different constants.
    VariableTerm v' -> do
      addVar v' -- Make sure the other variable is in the var map
      varMap <- use usVarMap
      uf     <- use usUF
      let ix  = varMap M.! v
      let ix' = varMap M.! v'
      let (mVal, uf')   = ufGet ix uf
      let (mVal', uf'') = ufGet ix' uf'
      case (mVal, mVal') of
        (Just c, Just c') -> guard (c == c') >> (usUF .= uf'')
        _                 -> usUF .= ufUnion ix ix' uf''
unifyTermS t (VariableTerm v)                 = unifyTermS (VariableTerm v) t

-- | Unify two atoms under a given @UnifyState@ context. Returns 'Nothing' if
-- the atoms cannot be unified.
unifyAtomS :: Ord a => Monad m
           => Atom' a -> Atom' a -> StateT (UnifyState a) (MaybeT m) ()
unifyAtomS (Atom p ts) (Atom p' ts')
  | p /= p'   = mzero
  | otherwise = mapM_ (uncurry unifyTermS) (zip ts ts')

-- | Unify two atoms. Returns a substitution that, when applied, would make the
-- two atoms equal. Returns 'Nothing' if the atoms cannot be unified.
--
-- The substitution is represented as a @Map@ from variables to terms.
unifyAtom :: Show a => Ord a => Atom' a -> Atom' a -> Maybe (Map a (Term' a))
unifyAtom a1 a2 = case mUS of
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
        Nothing -> do
          let (ix', uf'') = ufFind ix uf'
          _1 . at v ?= VariableTerm (ixToVarMap IM.! ix')
          _2 .= uf'' -- Update the union-find
        -- If the value is Just c, we map the variable to the constant c.
        Just c  -> do
          _1 . at v ?= ConstantTerm c
          _2 .= uf' -- Update the union-find
  where
    mUS = runIdentity . runMaybeT $ execStateT (unifyAtomS a1 a2) mkUnifyState

-- | Unify two terms. Returns a substitution that, when applied, would make the
-- two terms equal. Returns 'Nothing' if the terms cannot be unified.
--
-- The substitution is represented as a @Maybe@ since it is possible that the
-- two terms are already equal.
unifyTerm :: Eq a => Term' a -> Term' a -> Maybe (Maybe (a, Term' a))
unifyTerm (VariableTerm x) t'                = Just $ Just (x, t')
unifyTerm t t'@(VariableTerm _)              = unifyTerm t' t
unifyTerm (ConstantTerm c) (ConstantTerm c') = Nothing <$ guard (c == c')
{-# INLINE unifyTerm #-}

-- | Substitute a term with the given substitution map.
substituteTerm :: Ord a => Map a (Term' a) -> Term' a -> Term' a
substituteTerm m t@(VariableTerm x) = fromMaybe t (M.lookup x m)
substituteTerm _ t                  = t
{-# INLINE substituteTerm #-}

-- | Substitute an atom with the given substitution map.
substituteAtom :: Ord a => Map a (Term' a) -> Atom' a -> Atom' a
substituteAtom m (Atom p ts) = Atom p (map (substituteTerm m) ts)
{-# INLINE substituteAtom #-}

-- Apply the renaming function to all variables in the term.
renameTerm :: (a -> b) -> Term' a -> Term' b
renameTerm f (VariableTerm x) = VariableTerm $ f x
renameTerm _ (ConstantTerm c) = ConstantTerm c
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
renameProgram f p = p & clauses %~ map (renameClause f)
{-# INLINE renameProgram #-}

-- | Apply the renaming function to all variables in the query.
renamePQuery :: Ord b => (a -> b) -> PQuery' a -> PQuery' b
renamePQuery f q = q { _pqVariables  = S.map f (_pqVariables q)
                     , _pqAtoms      = map (renameAtom f) (_pqAtoms q) }
{-# INLINE renamePQuery #-}
