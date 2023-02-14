module Solver where

import           Data.Map (Map)
import qualified Data.Map as M
import           Program
import           Utility

solve :: Program -> PQuery -> [[Map String Term]]
solve (Program _ _ cs) (PQuery query) = worker M.empty query
  where
    worker sub []       = [[sub]]
    worker sub (t : ts) = do
      Clause { _clauseHead = h, _clauseBody = b } <- cs
      case h of
        Nothing -> []
        Just t' -> case unifyAtom t t' of
          Nothing   -> []
          Just sub' -> (sub :) <$> worker sub' (substituteAtom sub' <$> (b ++ ts))
