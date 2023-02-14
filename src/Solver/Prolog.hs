module Solver.Prolog where

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Program
import           Utility

-- | Given the "Program" and the Prolog query, return a list of variable
-- substitutions and all intermediate substitutions, each representing a
-- solution.
solve :: Program -> PQuery -> [(Solution, [Map String Term])]
solve (Program _ _ _ cs) (PQuery vars query)
  = (\subs -> (findVarSub subs, subs)) <$> worker (1 :: Int) M.empty query
  where
    subVar subs v   = (v, foldl (flip substituteTerm) (VariableTerm v) subs)
    findVarSub subs = Solution $ M.fromList (subVar subs <$> S.toList vars)

    worker _ sub []          = [[sub]]
    worker step sub (t : ts) = do
      (mH :?<- b) <- cs
      case mH of
        Nothing -> []
        Just t' -> do
          let rename = renameAtom (show step ++ "#")
          case unifyAtom t (rename t') of
            Nothing   -> []
            Just sub' -> (sub :)
                     <$> worker (step + 1) sub'
                                (substituteAtom sub' <$> (rename <$> b ++ ts))
