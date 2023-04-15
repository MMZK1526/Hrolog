{-# LANGUAGE TemplateHaskell #-}

-- | Internal helper functions for unification.
module Utility.Unifiers where

import           Control.Applicative
import           Control.Lens hiding (ix, un)
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Functor
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S

import           Internal.Program
import           Utility.UnionFind

data UnifyNode a = UnifyNode { unValue :: Maybe Constant
                             , unVars  :: Set a }

data UnifyState a = UnifyState { usNode    :: Int
                               , usVarMap  :: Map a Int
                               , usNodeMap :: IntMap (UnifyNode a) }

data UnifyState' a = UnifyState' { _usNode'   :: Int
                                 , _usVarMap' :: Map a Int
                                 , _usUF'     :: UnionFind Constant }
$(makeLenses ''UnifyState')

mkCUN :: Constant -> a -> UnifyNode a
mkCUN c var = UnifyNode (Just c) (S.singleton var)
{-# INLINE mkCUN #-}

mkUN :: Ord a => [a] -> UnifyNode a
mkUN = UnifyNode Nothing . S.fromList
{-# INLINE mkUN #-}

mergeUN :: Ord a => UnifyNode a -> UnifyNode a -> Maybe (UnifyNode a)
mergeUN un un' = do
  guard (fromMaybe True $ liftM2 (==) (unValue un) (unValue un'))
  return $ UnifyNode (unValue un <|> unValue un')
                     (S.union (unVars un) (unVars un'))
{-# INLINE mergeUN #-}

addUN :: Ord a => a -> UnifyNode a -> UnifyNode a
addUN var un = un { unVars = S.insert var (unVars un) }
{-# INLINE addUN #-}

emptyUS :: UnifyState a
emptyUS = UnifyState 0 M.empty IM.empty
{-# INLINE emptyUS #-}

emptyUS' :: Int -> UnifyState' a
emptyUS' = UnifyState' 0 M.empty . mkUnionFind
{-# INLINE emptyUS' #-}

newNodeIx :: Monad m => StateT (UnifyState' a) m Int
newNodeIx = do
  ix <- use usNode'
  usNode' += 1
  return ix
{-# INLINE newNodeIx #-}

addVar :: Ord a => Monad m => a -> StateT (UnifyState' a) m ()
addVar var = do
  varMap <- use usVarMap'
  case varMap M.!? var of
    Just _  -> return ()
    Nothing -> do
      ix <- newNodeIx
      usVarMap' . at var ?= ix
{-# INLINE addVar #-}

unifyTermS :: Ord a => Monad m
           => Term' a -> Term' a -> StateT (UnifyState' a) (MaybeT m) ()
unifyTermS (ConstantTerm c) (ConstantTerm c') = guard (c == c') $> ()
unifyTermS (VariableTerm v) t                 = do
  addVar v -- Make sure the variable is in the var map
  case t of
    -- If the term is a constant, we can just set the value of the variable
    -- to the constant, subjecting to the constraint that the variable is
    -- not already set to a different constant.
    ConstantTerm c -> do
      varMap <- use usVarMap'
      uf     <- use usUF'
      let ix = varMap M.! v
      let (mVal, uf') = ufGet ix uf
      case mVal of
        Nothing -> usUF' .= ufSet (Just c) ix uf'
        Just c' -> guard (c == c') >> (usUF' .= uf')
    -- If the term is another variable, we can just union the two variables,
    -- subjecting to the constraint that the two variables are not already
    -- set to different constants.
    VariableTerm v' -> do
      addVar v' -- Make sure the other variable is in the var map
      varMap <- use usVarMap'
      uf     <- use usUF'
      let ix  = varMap M.! v
      let ix' = varMap M.! v'
      let (mVal, uf')   = ufGet ix uf
      let (mVal', uf'') = ufGet ix' uf'
      case (mVal, mVal') of
        (Just c, Just c') -> guard (c == c') >> (usUF' .= uf'')
        _                 -> usUF' .= ufUnion ix ix' uf''
unifyTermS t (VariableTerm v)                 = unifyTermS (VariableTerm v) t

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

-- unifyAtom' :: Ord a => Atom' a -> Atom' a -> Maybe (Map a (Term' a))
-- unifyAtom' (Atom p ts) (Atom p' ts')
--   | p /= p'   = Nothing
--   | otherwise = case foldM worker (emptyUS' $ _predicateArity p) (zip ts ts') of
--     Nothing -> Nothing
--     Just uf -> Just $ foldl' (reducer uf) M.empty (ufEquivClasses $ uf ^. usUF')
--   where
--     worker us (t, t') = do
--       mUnified <- unifyTerm t t'
--       case mUnified of
--         Nothing          -> pure us
--         Just (var, term) -> do
--           let (i, us')    = case us ^. usVarMap'.at var of
--                 Just i'  -> (i', us)
--                 Nothing -> let i' = us ^. usNode'
--                            in (i', us & usNode' +~ 1 & usVarMap'.at var ?~ i')
--           let (mVal, uf') = ufGet i (us' ^. usUF')
--           case term of
--             ConstantTerm c -> case mVal of
--               Nothing -> pure $ us' & usUF' .~  ufSet (Just c) i uf'
--               Just c' -> guard (c == c') $> (us' & usUF' .~ uf')
--             VariableTerm v -> case us' ^. usVarMap'.at v of
--               Just i' -> do
--                 pure $ us' & usUF' .~ ufUnion i i' uf'
--               Nothing -> do
--                 let i'   = us' ^. usNode'
--                 let us'' = us' & usNode' +~ 1 & usVarMap'.at v ?~ i'
--                 pure $ us' & usUF' .~ ufSet mVal i uf'
--     reducer = undefined

-- | Unify two atoms. Returns a substitution that, when applied, would make the
-- two atoms equal. Returns 'Nothing' if the atoms cannot be unified.
--
-- The substitution is represented as a @Map@ from variables to terms.
unifyAtom :: Ord a => Atom' a -> Atom' a -> Maybe (Map a (Term' a))
unifyAtom (Atom p ts _) (Atom p' ts' _)
  | p /= p'   = Nothing
  | otherwise = case foldM worker emptyUS (zip ts ts') of
    Nothing -> Nothing
    Just us -> Just $ foldl' (reducer us) M.empty (M.assocs $ usVarMap us) 
  where
    reducer us sub (var, vid) = case usNodeMap us IM.!? vid of
      Nothing -> sub
      Just un -> case unValue un of
        Just c  -> M.insert var (ConstantTerm c) sub
        Nothing -> case fst <$> S.minView (unVars un) of
          Nothing -> sub
          Just v  -> M.insert var (VariableTerm v) sub
    worker us (t, t')         = do
      mUnified <- unifyTerm t t'
      case mUnified of
        Nothing      -> pure us
        Just unified -> do
          let n    = usNode us
          let vMap = usVarMap us
          let nMap = usNodeMap us
          case unified of
            (var, ConstantTerm c) -> case vMap M.!? var of
              Nothing -> do
                return us { usNode    = n + 1
                          , usVarMap  = M.insert var n vMap
                          , usNodeMap = IM.insert n (mkCUN c var) nMap }
              Just n' -> guard (Just c == unValue (nMap IM.! n')) $> us
            (var, VariableTerm v) -> case (vMap M.!? var, vMap M.!? v) of
              (Just n', Just n'') -> if n' == n''
                then pure us
                else do
                  un' <- mergeUN (nMap IM.! n') (nMap IM.! n'')
                  return us { usNodeMap = IM.insert n' un'
                                        $ IM.insert n'' un' nMap }
              (Nothing, Nothing)  -> do
                return us { usNode    = n + 1
                          , usVarMap  = M.insert var n $ M.insert v n vMap
                          , usNodeMap = IM.insert n (mkUN [var, v]) nMap }
              (Just n', Nothing)  -> do
                return us { usVarMap  = M.insert v n' vMap
                          , usNodeMap = IM.adjust (addUN v) n' nMap }
              (Nothing, Just n'') -> do
                return us { usVarMap  = M.insert var n'' vMap
                          , usNodeMap = IM.adjust (addUN var) n'' nMap }

-- | Substitute a term with the given substitution map.
substituteTerm :: Ord a => Map a (Term' a) -> Term' a -> Term' a
substituteTerm m t@(VariableTerm x) = fromMaybe t (M.lookup x m)
substituteTerm _ t                  = t
{-# INLINE substituteTerm #-}

-- | Substitute an atom with the given substitution map.
substituteAtom :: Ord a => Map a (Term' a) -> Atom' a -> Atom' a
substituteAtom m (Atom p ts avs) = Atom p (map (substituteTerm m) ts) avs
{-# INLINE substituteAtom #-}

-- Apply the renaming function to all variables in the term.
renameTerm :: (a -> b) -> Term' a -> Term' b
renameTerm f (VariableTerm x) = VariableTerm $ f x
renameTerm _ (ConstantTerm c) = ConstantTerm c
{-# INLINE renameTerm #-}

-- Apply the renaming function to all variables in the atom.
renameAtom :: (a -> b) -> Atom' a -> Atom' b
renameAtom f (Atom p ts avs) = Atom p (map (renameTerm f) ts) $ f <$> avs
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
