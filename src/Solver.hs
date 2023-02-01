module Solver where

import           Data.Map (Map)
import qualified Data.Map as M
import           Program
import           Utility

solve :: Program -> [Atom] -> [Map String Term]
solve = solve' M.empty

solve' :: Map String Term -> Program -> [Atom] -> [Map String Term]
solve' sub _ []                      = [sub]
solve' _ p@(Program _ _ cs) (t : ts) = do
  Clause { _clauseHead = h, _clauseBody = _ } <- cs
  case h of
    Nothing -> []
    Just t' -> case unifyAtom t t' of
      Nothing -> []
      Just sub' -> error (show sub') >> solve' sub' p (substituteAtom sub' <$> ts)
