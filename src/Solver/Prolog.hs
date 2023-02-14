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
  = findVarSub <$> worker (1 :: Int) M.empty query
  where
    subVar subs v   = (v, foldl (flip substituteTerm) (VariableTerm v) subs)
    findVarSub subs = (M.fromList (subVar subs <$> S.toList vars), subs)

    worker _ sub []          = [[sub]]
    worker step sub (t : ts) = do
      Clause { _clauseHead = h, _clauseBody = b } <- cs
      case h of
        Nothing -> []
        Just t' -> do
          let rename = renameAtom (show step ++ "#")
          case unifyAtom t (rename t') of
            Nothing   -> []
            Just sub' -> (sub :)
                     <$> worker (step + 1) sub'
                                (substituteAtom sub' <$> (rename <$> b ++ ts))
