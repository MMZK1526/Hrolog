module Solver where

import           Data.Map (Map)
import qualified Data.Map as M
import           Program
import           Utility

solve :: Program -> [Atom] -> [[Map String Term]]
solve = solve' M.empty

solve' :: Map String Term -> Program -> [Atom] -> [[Map String Term]]
solve' _ _ []                          = [[]]
solve' sub p@(Program _ _ cs) (t : ts) = do
  Clause { _clauseHead = h, _clauseBody = b } <- cs
  case h of
    Nothing -> []
    Just t' -> do
      case unifyAtom t t' of
        Nothing   -> []
        Just sub' -> (sub :) <$> solve' sub' p (substituteAtom sub' <$> (b ++ ts))
