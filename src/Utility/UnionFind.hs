{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | A Internal union-find data structure on @Int@ used for unification.
module Utility.UnionFind where

import           Control.Applicative
import           Control.Lens
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.List

-- | A union-find data structure on @Int@. Each representative (root) is
-- associated with a an optional value of type @a@.
data UnionFind a = UnionFind { _ufParent :: IntMap Int
                             , _ufRank   :: IntMap (Int, Maybe a)
                             , _ufSize   :: Int }
  deriving Show
$(makeLenses ''UnionFind)

-- | Create a new @UnionFind@ without elements.
mkUnionFind :: UnionFind a
mkUnionFind = UnionFind IM.empty IM.empty 0
{-# INLINE mkUnionFind #-}

-- | Add a new element to the @UnionFind@. The new element belongs to a
-- singleton set with itself as the root.
ufAdd :: UnionFind a -> UnionFind a
ufAdd uf = uf & ufParent.at n ?~ n
              & ufRank.at n   ?~ (1, Nothing)
              & ufSize        +~ 1
  where
    n = uf ^. ufSize
{-# INLINE ufAdd #-}

-- | Find the root (as well as the associated value) of the set containing @x@,
-- updating the parent pointers along the way.
--
-- It does not check if @x@ is a valid element.
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
--
-- It does not check if any of the elements are valid.
ufFinds :: [Int] -> UnionFind a -> ([Int], UnionFind a)
ufFinds xs uf = foldl' worker ([], uf) xs
  where
    worker (rs, uf') x = let (r, uf'') = ufFind x uf'
                         in  (r : rs, uf'')
{-# INLINE ufFinds #-}

-- | Adjust the value associated with the set containing @x@ using @f@,
-- returning the new value and the updated @UnionFind@.
--
-- It does not check if @x@ is a valid element.
ufAdjust :: (Maybe a -> Maybe a) -> Int -> UnionFind a -> (Maybe a, UnionFind a)
ufAdjust f x uf = (f a, uf' & ufRank.at r ?~ (rank, f a))
  where
    Just (rank, a) = uf' ^. ufRank.at r
    (r, uf')       = ufFind x uf
{-# INLINE ufAdjust #-}

-- | Get the value associated with the set containing @x@, returning the value
-- and the updated @UnionFind@.
--
-- It does not check if the element is valid.
ufGet :: Int -> UnionFind a -> (Maybe a, UnionFind a)
ufGet = ufAdjust id

-- | Apply @ufGet@ to each element of the list.
--
-- It does not check if any of the elements are valid.
ufGets :: [Int] -> UnionFind a -> ([Maybe a], UnionFind a)
ufGets xs uf = foldl' worker ([], uf) xs
  where
    worker (as, uf') x = let (a, uf'') = ufGet x uf'
                         in  (a : as, uf'')
{-# INLINE ufGets #-}

-- | Set the value associated with the set containing @x@.
--
-- It does not check if the element is valid.
ufSet :: Maybe a -> Int -> UnionFind a -> UnionFind a
ufSet v = (snd .) . ufAdjust (const v)

-- | Merge the sets containing @x@ and @y@, taking the one of the non-Nothing
-- value from the two sets as the new value for the union.
--
-- It does not check if the @x@ and @y@ are valid.
ufUnion :: Int -> Int -> UnionFind a -> UnionFind a
ufUnion x y uf
  | x == y = uf -- x and y are already in the same set
ufUnion x y uf = case rxr `compare` ryr of
  -- Use ry as parent of rx and increment rank of ry.
  LT -> uf'' & ufParent . at rx ?~ ry
             & ufRank . at ry %~ fmap (bimap (+ rxr) (<|> rxv))
  -- Use rx as parent of ry and increment rank of rx.
  _  -> uf'' & ufParent . at ry ?~ rx
             & ufRank . at rx %~ fmap (bimap (+ ryr) (<|> ryv))
  where
    (rx, uf')       = ufFind x uf  -- Find the root of the set containing x
    (ry, uf'')      = ufFind y uf' -- Find the root of the set containing y
    Just (rxr, rxv) = uf'' ^. ufRank . at rx -- Rank and value of rx
    Just (ryr, ryv) = uf'' ^. ufRank . at ry -- Rank and value of ry
