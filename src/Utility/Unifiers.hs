{-# LANGUAGE TemplateHaskell #-}

-- | Internal helper functions for unification.
module Utility.Unifiers where

import           Control.Applicative
import           Control.Lens hiding (un)
import           Control.Monad
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Functor
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S

import           Internal.Program

data UnifyNode a = UnifyNode { unValue :: Maybe Constant
                             , unVars  :: Set a }

data UnifyState a = UnifyState { usNode    :: Int
                               , usVarMap  :: Map a Int
                               , usNodeMap :: IntMap (UnifyNode a) }

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

unifyTerm :: Eq a => Term' a -> Term' a -> Maybe (Maybe (a, Term' a))
unifyTerm (VariableTerm x) t'                = Just $ Just (x, t')
unifyTerm t t'@(VariableTerm _)              = unifyTerm t' t
unifyTerm (ConstantTerm c) (ConstantTerm c') = Nothing <$ guard (c == c')
{-# INLINE unifyTerm #-}

unifyAtom :: Ord a => Atom' a -> Atom' a -> Maybe (Map a (Term' a))
unifyAtom (Atom p ts) (Atom p' ts')
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

substituteTerm :: Ord a => Map a (Term' a) -> Term' a -> Term' a
substituteTerm m t@(VariableTerm x) = fromMaybe t (M.lookup x m)
substituteTerm _ t                  = t
{-# INLINE substituteTerm #-}

substituteAtom :: Ord a => Map a (Term' a) -> Atom' a -> Atom' a
substituteAtom m (Atom p ts) = Atom p (map (substituteTerm m) ts)
{-# INLINE substituteAtom #-}

renameTerm :: (a -> b) -> Term' a -> Term' b
renameTerm f (VariableTerm x) = VariableTerm $ f x
renameTerm _ (ConstantTerm c) = ConstantTerm c
{-# INLINE renameTerm #-}

renameAtom :: (a -> b) -> Atom' a -> Atom' b
renameAtom f (Atom p ts) = Atom p (map (renameTerm f) ts)
{-# INLINE renameAtom #-}

renameClause :: (a -> b) -> Clause' a -> Clause' b
renameClause f (Clause h b) = Clause (renameAtom f <$> h) (map (renameAtom f) b)
{-# INLINE renameClause #-}

renameProgram :: (a -> b) -> Program' a -> Program' b
renameProgram f p = p & clauses %~ map (renameClause f)
{-# INLINE renameProgram #-}

renamePQuery :: Ord b => (a -> b) -> PQuery' a -> PQuery' b
renamePQuery f q = q { _pqVariables  = S.map f (_pqVariables q)
                     , _pqAtoms      = map (renameAtom f) (_pqAtoms q) }
{-# INLINE renamePQuery #-}


--------------------------------------------------------------------------------
-- Union Find on Int
--------------------------------------------------------------------------------

data UnionFind = UnionFind { _ufParent :: IntMap Int
                           , _ufRank   :: IntMap Int }
$(makeLenses ''UnionFind)

-- | Create a new @UnionFind@ with @n@ elements.
mkUnionFind :: Int -> UnionFind
mkUnionFind n = UnionFind (IM.fromDistinctAscList $ zip [0..n - 1] [0..n - 1])
                          (IM.fromDistinctAscList $ zip [0..n - 1] (repeat 1))
{-# INLINE mkUnionFind #-}

-- | Find the root of the set containing @x@, updating the parent pointers
-- along the way.
ufFind :: Int -> UnionFind -> (Int, UnionFind)
ufFind x uf = case uf ^. ufParent.at x of
  -- Should never happen if the "UnionFind" is constructed by "mkUnionFind".
  Nothing -> error "Internal Error: UnionFind.ufFind: element not found"
  Just p  -> if p == x
    then (x, uf) -- x is already the root
    -- Recursively find the root and updated parent pointers.
    else let (r, uf') = ufFind p uf
         in  (r, uf' & ufParent.at x ?~ r) -- Update parent pointer
{-# INLINE ufFind #-}

-- | Merge the sets containing @x@ and @y@.
ufUnion :: Int -> Int -> UnionFind -> UnionFind
ufUnion x y uf
  | rx == ry   = uf'' -- x and y are already in the same set
  | xLessThanY = uf'' & ufParent . at rx ?~ ry -- Use ry as parent of rx
                      & ufRank . at ry %~ fmap succ -- increment rank of ry
  | otherwise  = uf'' & ufParent . at ry ?~ rx -- Use rx as parent of ry
                      & ufRank . at rx %~ fmap succ -- increment rank of rx
  where
    (rx, uf')  = ufFind x uf  -- Find the root of the set containing x
    (ry, uf'') = ufFind y uf' -- Find the root of the set containing y
    -- Rank of rx is less than the rank of ry
    xLessThanY = uf'' ^. ufRank . at rx < uf'' ^. ufRank . at ry
