{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Solver.Prolog where

import           Control.Monad
import           Control.Monad.Trans.State
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
  = (\subs -> (findVarSub subs, subs)) <$> evalState (worker (1 :: Int) M.empty query) ()
  where
    subVar subs v   = (v, foldl (flip substituteTerm) (VariableTerm v) subs)
    findVarSub subs = Solution $ M.fromList (subVar subs <$> S.toList vars)

    worker _ sub []          = pure [[sub]]
    worker step sub (t : ts) = fmap concat . forM cs $ \(mH :?<- b) -> case mH of
      Nothing -> pure []
      Just a' -> do
        let rename = renameAtom (show step ++ "#")
        case unifyAtom t (rename a') of
          Nothing   -> pure []
          Just sub' -> do
            rest <- worker (step + 1) sub'
                           (substituteAtom sub' <$> (map rename b ++ ts))
            return $ (sub :) <$> rest
