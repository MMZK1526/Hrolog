module Solver.Prolog where

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Program
import           Utility

-- | Given the "Program" and the Prolog query, return a list of variable
-- substitutions and all intermediate substitutions, each representing a
-- solution.
solve :: Program -> PQuery -> [(Map String Term, [Map String Term])]
solve (Program _ _ _ cs) (PQuery vars query)
  = findVarSub <$> worker M.empty query
  where
    findVarSub subs = (M.fromList ((\v -> (v, foldl (flip substituteTerm) (VariableTerm v) subs)) <$> S.toList vars), subs)
    worker sub []       = [[sub]]
    worker sub (t : ts) = do
      Clause { _clauseHead = h, _clauseBody = b } <- cs
      case h of
        Nothing -> []
        Just t' -> case unifyAtom t t' of
          Nothing   -> []
          Just sub' -> (sub :) <$> worker sub' (substituteAtom sub' <$> (b ++ ts))
