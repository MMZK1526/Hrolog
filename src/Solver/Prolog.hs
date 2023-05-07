{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Solver.Prolog (prettifySolution, solve, solveIO, solveS) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Internal.Program
import           Utility.PP
import           Utility.Unifiers

-- | Transform a "tagged variable" into its @Text@ representation.
--
-- It is necessary since during solving, we need to perform alpha-conversion to
-- avoid variable capture. Therefore we "tag" the variables with a unique number
-- (namely the step count) to avoid name clashes. In other words, during
-- solving, we are actually dealing with "tagged @Text@"s. Upon reaching the
-- solution, we need to "untag" them to get the original variables (creating
-- fresh variables if necessary).
untag :: (Maybe Int, Text) -> Text
untag (Nothing, s) = s
untag (Just i, s)  = T.concat [pShow i, "#", s]

-- | The type of query used during solving. It tagged each variable with an
-- optional integer, so that each renaming is unique.
type SolvePQuery = PQuery' (Maybe Int, Text)

-- | The type of clauses used during solving. It tagged each variable with an
-- optional integer, so that each renaming is unique.
type SolveClause = Clause' (Maybe Int, Text)

-- | The state used by the Prolog solver.
--
-- It tabulates some information to increase performance.
-- 
-- @pProven@ is the set of proven atoms. When these atoms are encountered again
-- (up to alpha-conversion) in the derivation, they can be automatically proven.
data PState = PState
  { _pStep  :: Int  -- ^ Number of steps
  , _pIsBT  :: Bool -- ^ If under backtracking
  -- ^ Rules by the head predicate
  , _pRules :: Map (Maybe Predicate) [SolveClause]
  }
$(makeLenses ''PState)

newPState :: [SolveClause] -> PState
newPState cs = PState 1 False (foldr' insertRule M.empty cs)
  where
    insertRule c@(h :?<- _) m = case m M.!? p of
      Nothing  -> M.insert p [c] m
      Just cs' -> M.insert p (c : cs') m
      where
        p = view atomPredicate <$> h

-- | Given the @Program@ and the Prolog query, pure a list of variable
-- substitutions and all intermediate substitutions, each representing a
-- solution.
--
-- Solutions are evaluated in the normal Prolog order. The evaluation is lazy,
-- so it is always possible to get the first few solutions even if there are
-- infinitely many.
solve :: Program -> PQuery -> [Solution]
solve p q = runIdentity $ solveS pure (pure ()) pure p q

-- | Similar to @solve@, but prints out each step.
solveIO :: Program -> PQuery -> IO [Solution]
solveIO = solveS onNewStep onFail onBacktractEnd
  where
    untagged    = renamePQuery untag
    onNewStep q = do
      steps <- use pStep
      lift $ putStrLn (concat ["Step ", show steps, ":"])
      lift $ case _pqAtoms q of
        [] -> T.putStrLn "Unification succeeded.\n"
        _  -> T.putStrLn
            $ T.concat ["Current query: ", pShow (untagged q), "\n"]
    onFail      = do
      steps <- use pStep
      lift $ T.putStrLn (T.concat ["Step ", pShow steps, ":"])
      lift $ T.putStrLn "Unification failed.\n"
    onBacktractEnd q = lift . T.putStrLn 
      $ T.concat["Backtracked to the query ", pShow (untagged q), "\n"]

-- | The main function of the Prolog solver.
--
-- It takes three functions as arguments:
-- 1. @onNewStep@ is called when a new step is taken. It takes the current
-- @PQuery@ as an argument.
-- 2. @onFail@ is called whenever the unification fails.
-- 3. @onBacktrackEnd@ is called when backtracking ends. It takes the current
-- @PQuery@ as an argument.
--
-- The fourth argument is the @Program@, and the fifth argument is the original
-- query.
--
-- The pure value is a list of @Solution@s, which describes how each variable
-- in the @PQuery@ is mapped to a term. If the pure value is empty, then there
-- is no solution.
solveS :: Monad m
       => (SolvePQuery -> StateT PState m a) -> StateT PState m b
       -> (SolvePQuery -> StateT PState m a) -> Program -> PQuery
       -> m [Solution]
-- "worker" is the main function of the solver. It takes the current
-- substitution, the current query, and returns a list of solutions, each is a
-- series of substitutions that happened at each step.
--
-- Once we have the solutions in the form of series of substitutions, we apply
-- them to the variables in the query to get the monolithic substitutions, which
-- describe the solutions.
solveS onNewStep onFail onBacktrackEnd (Program _ _ _ cs) pquery
  = fmap (map (optimiseSub . findVarSub))
         (evalStateT (worker M.empty query') $ newPState cs')
  where
    cs'                 = renameClause (Nothing ,) <$> cs -- tagged clauses
    PQuery vars' query' = renamePQuery (Nothing ,) pquery -- tagged query
    -- Apply a series of substitutions to a variable.
    subVar subs v       = (v, foldl (flip substituteTerm) (VariableTerm v) subs)
    -- Find the relavant substitutions only on variables in the query.
    findVarSub subs     = subVar subs <$> S.toList vars'

    -- Once the substitutions for the query variables are found, we want to
    -- perform two optimisations:
    -- 1. If a variable is mapped to a "renamed" variable, e.g. "1#A", we want
    -- to change it to a variable with a valid name.
    -- 2. If a variable is mapped to itself, we can remove the substitution.
    optimiseSub subs = Solution $ M.fromList optimisedSubs
      where
        -- TODO: If there are still "renamed" variables, create a fresh variable
        -- and substitute it in.
        optimisedSubs = filter (\(v, term) -> VariableTerm v /= term) $
          bimap snd (renameTerm snd . substituteTerm validReps) <$> subs
        validReps     = foldr reducer M.empty subs

        reducer (iv@(Nothing, _), term) cur = case term of
          VariableTerm iv'@(Just _, _) -> M.insert iv' (VariableTerm iv) cur
          _                            -> cur
        reducer _ cur                       = cur

    -- Base case: if the current query is empty, we have found a solution.
    worker sub []         = do
      void $ onNewStep (PQuery vars' [])
      pIsBT .= True
      pStep += 1
      pure [[sub]]
    -- Take the first atom in the query, and try to unify it with each clause.
    worker sub q@(t : ts) = do
      allRules <- use pRules
      let rules = fromMaybe [] $ allRules M.!? Just (t ^. atomPredicate)
      result   <- fmap concat . forM rules $ \(mH :?<- b) -> case mH of
        Nothing -> pure [] -- Ignore constraints (for now)
        Just h  -> do      -- Try to unify with the head "h"
          (step, isBT) <- liftM2 (,) (use pStep) (use pIsBT)
          when isBT $ onBacktrackEnd (PQuery vars' q) >> pIsBT .= False
          -- A rename function that tags the variable with the current step
          -- count, so that it is guaranteed to be unique.
          let rename = renameAtom (first (const $ Just step))
          case unifyAtom t (rename h) of
            Nothing   -> pure [] -- Unification failed
            Just sub' -> do      -- Unification succeeded
              onNewStep (PQuery vars' q) >> pStep += 1
              -- Add the body of the clause to the query, substituting the
              -- variables using the substitution we just found.
              let nextQuery = map (substituteAtom sub') (map rename b ++ ts)
              -- Recursively solve the new query.
              rest <- worker sub' nextQuery
              pure $ (sub :) <$> rest
      -- If "result" is empty, it means we didn't find any solution in the
      -- current branch. Backtracking happens automatically via recursion.
      when (null result) $ onFail >> pStep += 1 >> pIsBT .= True
      pure result
