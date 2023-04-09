{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | A Internal union-find data structure on @Int@ used for unification.
module Utility.UnionFind where

import           Control.Lens
import           Data.Bifunctor
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List

-- | A union-find data structure on @Int@. Each representative (root) is
-- associated with a an optional value of type @a@.
data UnionFind a = UnionFind { _ufParent :: IntMap Int
                             , _ufRank   :: IntMap (Int, Maybe a) }
$(makeLenses ''UnionFind)

-- | Create a new @UnionFind@ with @n@ elements.
mkUnionFind :: Int -> UnionFind a
mkUnionFind n
  = UnionFind (IM.fromDistinctAscList $ zip [0..n - 1] [0..n - 1])
              (IM.fromDistinctAscList $ zip [0..n - 1] (repeat (1, Nothing)))
{-# INLINE mkUnionFind #-}

-- | Find the root (as well as the associated value) of the set containing @x@,
-- updating the parent pointers along the way.
ufFind :: Int -> UnionFind a -> (Int, UnionFind a)
ufFind x uf = case uf ^. ufParent.at x of
  -- The "Nothing" case should be unreachable.
  Just p  -> if p == x
    then (x, uf) -- x is already the root
    -- Recursively find the root and updated parent pointers.
    else let (r, uf') = ufFind p uf
         in  (r, uf' & ufParent.at x ?~ r) -- Update parent pointer
{-# INLINE ufFind #-}

-- | Apply @ufFind@ to each element of the list.
ufFinds :: [Int] -> UnionFind a -> ([Int], UnionFind a)
ufFinds xs uf = foldl' worker ([], uf) xs
  where
    worker (rs, uf') x = let (r, uf'') = ufFind x uf'
                         in  (r : rs, uf'')
{-# INLINE ufFinds #-}

-- | Adjust the value associated with the set containing @x@ using @f@,
-- returning the new value and the updated @UnionFind@.
ufAdjust :: (Maybe a -> Maybe a) -> Int -> UnionFind a -> (Maybe a, UnionFind a)
ufAdjust f x uf = (f a, uf' & ufRank.at r ?~ (rank, f a))
  where
    Just (rank, a) = uf' ^. ufRank.at r
    (r, uf')       = ufFind x uf
{-# INLINE ufAdjust #-}

-- | Get the value associated with the set containing @x@, returning the value
-- and the updated @UnionFind@.
ufGet :: Int -> UnionFind a -> (Maybe a, UnionFind a)
ufGet = ufAdjust id

-- | Apply @ufGet@ to each element of the list.
ufGets :: [Int] -> UnionFind a -> ([Maybe a], UnionFind a)
ufGets xs uf = foldl' worker ([], uf) xs
  where
    worker (as, uf') x = let (a, uf'') = ufGet x uf'
                         in  (a : as, uf'')
{-# INLINE ufGets #-}

-- | Set the value associated with the set containing @x@.
ufSet :: Maybe a -> Int -> UnionFind a -> UnionFind a
ufSet v = (snd .) . ufAdjust (const v)

-- | Merge the sets containing @x@ and @y@.
ufUnion :: Int -> Int -> UnionFind a -> UnionFind a
ufUnion x y uf = case rxr `compare` ryr of
  -- Use ry as parent of rx and increment rank of ry.
  LT -> uf'' & ufParent . at rx ?~ ry
             & ufRank . at ry %~ fmap (first succ)
  -- Use rx as parent of ry and increment rank of rx.
  GT -> uf'' & ufParent . at ry ?~ rx
             & ufRank . at rx %~ fmap (first succ)
  -- x and y are already in the same set.
  EQ -> uf''
  where
    (rx, uf')  = ufFind x uf  -- Find the root of the set containing x
    (ry, uf'') = ufFind y uf' -- Find the root of the set containing y
    Just rxr   = fst <$> uf'' ^. ufRank . at rx -- Rank of rx
    Just ryr   = fst <$> uf'' ^. ufRank . at ry -- Rank of ry

-- | Find all equivalence classes of the @UnionFind@, toegether with their
-- associated value.
ufEquivClasses :: UnionFind a -> [([Int], Maybe a)]
ufEquivClasses uf
  = IM.elems $ IM.fromListWith (\(x1s, a) (x2s, _) -> (x1s ++ x2s, a))
                               [(r, ([x], a)) | (x, a, r) <- elemValReps]
  where
    allElems    = IM.keys $ uf ^. ufParent
    elemValReps = zip3 (fst $ ufFinds allElems uf)
                       (fst $ ufGets allElems uf)
                       allElems 
