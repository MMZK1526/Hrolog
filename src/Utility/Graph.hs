{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

-- | A helper module for detecting cycles in a graph.
module Utility.Graph where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Maybe

newtype Graph = Graph (IntMap [Int])
  deriving (Eq, Show)

-- | Return @True@ if the graph contains a cycle.
checkCycle :: Graph -> Bool
checkCycle (Graph g)
  = isJust (execStateT (forM_ (IM.keys g) go) (IS.empty, IS.empty))
  where
    go node = do
      visited  <- use _1
      entering <- use _2
      if
        | IS.member node visited  -> pure ()
        | IS.member node entering -> lift Nothing
        | otherwise               -> do
          _1 %= IS.insert node
          _2 %= IS.insert node
          forM_ (g IM.! node) go
          _2 %= IS.delete node
