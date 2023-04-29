{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

-- | A helper module for detecting cycles in a graph.
module Utility.Graph where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import           Data.Maybe

newtype Graph = Graph (IntMap [Int])

-- | pure @True@ if the graph contains a cycle.
hasCycle :: Graph -> Bool
hasCycle (Graph g)
  = isNothing (execStateT (forM_ (IM.keys g) go) (IS.empty, IS.empty))
  where
    go node = do
      visited  <- use _1
      entering <- use _2
      if
        | IS.member node visited  -> pure ()
        | IS.member node entering -> lift Nothing
        | otherwise               -> do
          _2 %= IS.insert node
          forM_ (fromMaybe [] $ g IM.!? node) go
          _2 %= IS.delete node
          _1 %= IS.insert node
