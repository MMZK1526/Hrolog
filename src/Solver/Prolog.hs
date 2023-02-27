{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Solver.Prolog where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Functor
import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Program
import           Utility

-- | The state used by the Prolog solver.
--
-- It tabulates some information to increase performance.
-- 
-- "pProven" is the set of proven atoms. When these atoms are encountered again
-- (up to alpha-conversion) in the derivation, they can be automatically proven.
data PState = PState
  { pStep    :: Int                 -- ^ Number of steps
  , pProven  :: Set Atom            -- ^ What's been proven so far
  , pTry     :: Map Atom (Set Atom) -- ^ What we are trying to prove
  , pLeadsTo :: Map Atom (Set Atom) -- ^ What we can deduce
  }

newPState :: PState
newPState = PState 1 S.empty M.empty M.empty

-- | Given the "Program" and the Prolog query, return a list of variable
-- substitutions and all intermediate substitutions, each representing a
-- solution.
solve :: Program -> PQuery -> [(Solution, [Map String Term])]
solve p q = runIdentity $ solveS pure p q

-- | Similar to "solve", but prints out each step.
solveIO :: Program -> PQuery -> IO [(Solution, [Map String Term])]
solveIO = solveS worker
  where
    worker q = do
      steps <- gets pStep
      lift $ putStrLn (concat ["Step ", show steps, ":"])
      lift $ pPrint q

solveS :: Monad m => (PQuery -> StateT PState m a) -> Program -> PQuery
       -> m [(Solution, [Map String Term])]
solveS onNewStep (Program _ _ _ cs) (PQuery vars query)
  = fmap (map (\subs -> (findVarSub subs, subs)))
         (evalStateT (worker M.empty query) newPState)
  where
    subVar subs v   = (v, foldl (flip substituteTerm) (VariableTerm v) subs)
    findVarSub subs = Solution $ M.fromList (subVar subs <$> S.toList vars)

    worker sub []         = onNewStep (PQuery vars []) $> [[sub]]
    worker sub q@(t : ts) = fmap concat . forM cs $ \(mH :?<- b) -> case mH of
      Nothing -> pure []
      Just h  -> do
        step <- gets pStep
        let rename = renameAtom (show step ++ "#")
        case unifyAtom t (rename h) of
          Nothing   -> pure []
          Just sub' -> do
            void $ onNewStep (PQuery vars q)
            modify (\s -> s { pStep = step + 1 })
            let nextQuery = substituteAtom sub' <$> (map rename b ++ ts)
            rest <- worker sub' nextQuery
            return $ (sub :) <$> rest
