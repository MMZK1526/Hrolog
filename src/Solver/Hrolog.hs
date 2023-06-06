{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Solver.Hrolog (
  solve,
  solveOne,
  solveIO,
  solveS
) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
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

data BacktrackState = NoBacktrack | BacktrackOnSuccess | BacktrackOnFailure
  deriving (Eq, Show)

-- | The state used by the Prolog solver.
--
-- It tabulates some information to increase performance.
-- 
-- @pProven@ is the set of proven atoms. When these atoms are encountered again
-- (up to alpha-conversion) in the derivation, they can be automatically proven.
data PState = PState
  { _pStep  :: Int            -- ^ Number of steps
  , _pIsBT  :: BacktrackState -- ^ If under backtracking
  -- ^ Rules by the head predicate
  , _pRules :: Map (Maybe Predicate) [SolveClause]
  , _pEnd   :: Bool           -- ^ If the user terminates the query
  }
$(makeLenses ''PState)

newPState :: [SolveClause] -> PState
newPState cs = PState 1 NoBacktrack (foldr' insertRule M.empty cs) False
  where
    insertRule c@(h :?<- _) m = case m M.!? p of
      Nothing  -> M.insert p [c] m
      Just cs' -> M.insert p (c : cs') m
      where
        p = view atomPredicate <$> h

-- | Given the @Program@ and the Prolog query, return a list of variable
-- substitutions and all intermediate substitutions, each representing a
-- solution.
--
-- Solutions are evaluated in the normal Prolog order. The evaluation is lazy,
-- so it is always possible to get the first few solutions even if there are
-- infinitely many.
solve :: Program -> PQuery -> [Solution]
solve p = runIdentity
        . solveS False (\_ _ -> pure True) (pure ()) (const $ pure ()) p

-- | Given the @Program@ and the Prolog query, return the first solution.
solveOne :: Program -> PQuery -> Maybe Solution
solveOne p = listToMaybe . runIdentity
           . solveS True (\_ _ -> pure True) (pure ()) (const $ pure ()) p

-- | Similar to @solve@, but prints out each step. It takes an additional
-- action which indicates if the solver should continue.
solveIO :: IO Bool -> Program -> PQuery -> IO [Solution]
solveIO cont = solveS False onNewStep onFail onBacktractEnd
  where
    untagged             = renamePQuery untag
    onNewStep Nothing _  = pure True
    onNewStep (Just r) q = do
      steps <- use pStep
      lift $ putStrLn (concat ["Step ", show steps, ":"])
      lift $ T.putStrLn $ T.concat [ "Unified with the rule "
                                   , pShow (renameClause untag r)
                                   , "\n" ]
      lift $ case _pqAtoms q of
        [] -> T.putStrLn "Unification succeeded.\n"
        _  -> T.putStrLn
            $ T.concat ["Current query: ", pShow (untagged q), "\n"]
      lift cont
    onFail               = do
      steps <- use pStep
      lift $ T.putStrLn (T.concat ["Step ", pShow steps, ":"])
      lift $ T.putStrLn "Unification failed with all rules.\n"
    onBacktractEnd q = do
      lift . T.putStrLn $ T.concat [ "Backtracked to the query "
                                   , pShow (untagged q), "\n" ]

-- | The main function of the Prolog solver.
--
-- The first argument, @quickQuit@, is a flag that indicates whether the solver
-- should quit immediately after finding the first solution.
--
-- It then takes three functions as arguments:
-- 1. @onNewStep@ is called when a new step is taken. It takes the current
--    matching rule and the current @PQuery@ as arguments. It returns a @Bool@,
--    which indicates whether the solver should continue.
-- 2. @onFail@ is called whenever the unification fails.
-- 3. @onBacktrackEnd@ is called when backtracking ends. It takes the current
--    @PQuery@ as an argument.
--
-- The fourth argument is the @Program@, and the fifth argument is the original
-- query.
--
-- The pure value is a list of @Solution@s, which describes how each variable
-- in the @PQuery@ is mapped to a term. If the pure value is empty, then there
-- is no solution.
solveS :: Monad m
       => Bool
       -> (Maybe SolveClause -> SolvePQuery -> StateT PState m Bool)
       -> StateT PState m ()
       -> (SolvePQuery -> StateT PState m ()) -> Program -> PQuery
       -> m [Solution]
-- "worker" is the main function of the solver. It takes the current
-- substitution, the current query, and returns a list of solutions, each is a
-- series of substitutions that happened at each step.
--
-- Once we have the solutions in the form of series of substitutions, we apply
-- them to the variables in the query to get the monolithic substitutions, which
-- describe the solutions.
solveS quickQuit onNewStep onFail onBacktrackEnd (Program _ _ _ cs) pquery
  = fmap (map (optimiseSub . findVarSub))
         (evalStateT (worker quickQuit M.empty query') $ newPState cs')
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
    worker _ sub []                       = do
      void $ onNewStep Nothing (PQuery vars' [])
      pIsBT .= BacktrackOnSuccess
      pStep += 1
      pure [[sub]]
    -- Take the first atom in the query, and try to unify it with each clause.
    worker qq sub ((Atom True p ts) : as) = do
      pIsBT .= NoBacktrack
      subResult <- worker True M.empty [Atom False p ts]
      -- TODO: Add handler for completing a sub-query for negation.
      case subResult of
        [] -> worker qq sub as
        _  -> pIsBT .= BacktrackOnFailure >> pure []
    worker qq sub q@(a : as)              = do
      allRules <- use pRules
      let rules = fromMaybe [] $ allRules M.!? Just (a ^. atomPredicate)
      result   <- fmap concat . forMBreak rules $ \r@(mH :?<- b) -> case mH of
        Nothing -> pure [] -- Ignore constraints (for now)
        Just h  -> do      -- Try to unify with the head "h"
          (step, isBT) <- liftM2 (,) (use pStep) (use pIsBT)
          when (isBT /= NoBacktrack) $ do
            lift (onBacktrackEnd (PQuery vars' q))
            pIsBT .= NoBacktrack
          guard (isBT /= BacktrackOnFailure || not qq)
          -- A rename function that tags the variable with the current step
          -- count, so that it is guaranteed to be unique.
          let rename = renameAtom (first (const $ Just step))
          case unifyAtom a (rename h) of
            Nothing   -> pure [] -- Unification failed
            Just sub' -> do      -- Unification succeeded
              -- Add the body of the clause to the query, substituting the
              -- variables using the substitution we just found.
              let nextQuery = map (substituteAtom sub') (map rename b ++ as)
              end <- use pEnd
              unless end $ do
                continue <- lift (onNewStep (Just r) (PQuery vars' nextQuery))
                pEnd .= not continue
                guard continue
              pStep += 1
              -- Recursively solve the new query.
              rest <- lift $ worker False sub' nextQuery
              pure $ (sub :) <$> rest
      -- If "result" is empty, it means we didn't find any solution in the
      -- current branch. Backtracking happens automatically via recursion.
      isBT <- use pIsBT
      end <- use pEnd
      when (not end && null result && isBT /= BacktrackOnFailure) $
        onFail >> pStep += 1 >> pIsBT .= BacktrackOnFailure
      pure result

forMBreak :: Monad m => [a] -> (a -> MaybeT m b) -> m [b]
forMBreak [] _ = pure []
forMBreak (x : xs) f = do
  r <- runMaybeT $ f x
  case r of
    Nothing -> pure []
    Just y  -> (y :) <$> forMBreak xs f
