module Solver where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Program
import           Utility

solve :: Program -> Atom -> [Map String Term]
solve = solve' M.empty

solve' :: Map String Term -> Program -> Atom -> [Map String Term]
solve' _ (Program _ _ cs) t = catMaybes $ do
  Clause { _clauseHead = h, _clauseBody = _ } <- cs
  return $ case h of
    Nothing -> Nothing
    Just t' -> unifyAtom t t'
