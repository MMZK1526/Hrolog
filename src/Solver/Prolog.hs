{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Solver.Prolog where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Program
import           Utility

-- | The state used by the Prolog solver.
--
-- TODO: Explanation.
data PState = PState
  { pStep    :: Int                 -- ^ Number of steps
  , pProven  :: Set Atom            -- ^ What's been proven so far
  , pTry     :: Map Atom (Set Atom) -- ^ What we are trying to prove
  , pLeadsTo :: Map Atom (Set Atom) -- ^ What we can deduce
  }

emptyState :: PState
emptyState = PState 1 S.empty M.empty M.empty

-- | Given the "Program" and the Prolog query, return a list of variable
-- substitutions and all intermediate substitutions, each representing a
-- solution.
solve :: Program -> PQuery -> [(Solution, [Map String Term])]
solve (Program _ _ _ cs) (PQuery vars query)
  = (\subs -> (findVarSub subs, subs)) <$> evalState (worker M.empty query) emptyState
  where
    subVar subs v   = (v, foldl (flip substituteTerm) (VariableTerm v) subs)
    findVarSub subs = Solution $ M.fromList (subVar subs <$> S.toList vars)

    worker sub []       = pure [[sub]]
    worker sub (t : ts) = fmap concat . forM cs $ \(mH :?<- b) -> case mH of
      Nothing -> pure []
      Just a' -> do
        step <- gets pStep
        let rename = renameAtom (show step ++ "#")
        case unifyAtom t (rename a') of
          Nothing   -> pure []
          Just sub' -> do
            modify (\s -> s { pStep = step + 1 })
            rest <- worker sub' (substituteAtom sub' <$> (map rename b ++ ts))
            return $ (sub :) <$> rest
