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

-- | Unify two atoms. Returns a substitution that, when applied, would make the
-- two atoms equal. Returns 'Nothing' if the atoms cannot be unified.
--
-- The substitution is represented as a @Map@ from variables to terms.
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


--------------------------------------------------------------------------------
-- Union-Find on Int
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

-- | Apply @ufFind@ to each element of the list.
ufFinds :: [Int] -> UnionFind -> ([Int], UnionFind)
ufFinds xs uf = foldl' worker ([], uf) xs
  where
    worker (rs, uf') x = let (r, uf'') = ufFind x uf'
                         in  (r : rs, uf'')
{-# INLINE ufFinds #-}

-- | Merge the sets containing @x@ and @y@.
ufUnion :: Int -> Int -> UnionFind -> UnionFind
ufUnion x y uf = case rxr `compare` ryr of
  -- Use ry as parent of rx and increment rank of ry.
  LT -> uf'' & ufParent . at rx ?~ ry
             & ufRank . at ry %~ fmap succ
  -- Use rx as parent of ry and increment rank of rx.
  GT -> uf'' & ufParent . at ry ?~ rx
             & ufRank . at rx %~ fmap succ
  -- x and y are already in the same set.
  EQ -> uf''
  where
    (rx, uf')  = ufFind x uf  -- Find the root of the set containing x
    (ry, uf'') = ufFind y uf' -- Find the root of the set containing y
    rxr        = uf'' ^. ufRank . at rx -- Rank of rx
    ryr        = uf'' ^. ufRank . at ry -- Rank of ry

-- | Find all equivalence classes of the @UnionFind@.
ufEquivClasses :: UnionFind -> [[Int]]
ufEquivClasses uf
  = IM.elems $ IM.fromListWith (++) [(r, [x]) | (x, r) <- elemToReps]
  where
    allElems   = IM.keys $ uf ^. ufParent
    elemToReps = zip (fst $ ufFinds allElems uf) allElems 
{-# INLINE ufEquivClasses #-}
