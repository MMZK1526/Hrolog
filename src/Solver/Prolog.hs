{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Solver.Prolog (prettyPrintSolution, solve, solveIO, solveS) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Internal.Program
import           Utility.PP
import           Utility.Unifiers

-- | A type class for a "tagged @String@".
--
-- It is necessary since during solving, we need to perform alpha-conversion to
-- avoid variable capture. Therefore we "tag" the variables with a unique number
-- (namely the step count) to avoid name clashes. In other words, during
-- solving, we are actually dealing with "tagged @String@"s. Upon reaching the
-- solution, we need to "untag" them to get the original variables (creating
-- fresh variables if necessary).
class Tagged a where
  untag :: a -> String

instance Tagged String where
  untag :: String -> String
  untag = id

instance Tagged (Maybe Int, String) where
  untag :: (Maybe Int, String) -> String
  untag (Nothing, s) = s
  untag (Just i, s)  = concat [show i, "#", s]

-- | The type of query used during solving. It tagged each variable with an
-- optional integer, so that each renaming is unique.
type SolvePQuery = PQuery' (Maybe Int, String)

-- | The state used by the Prolog solver.
--
-- It tabulates some information to increase performance.
-- 
-- @pProven@ is the set of proven atoms. When these atoms are encountered again
-- (up to alpha-conversion) in the derivation, they can be automatically proven.
data PState = PState
  { _pStep    :: Int                 -- ^ Number of steps
  , _pProven  :: Set Atom            -- ^ What's been proven so far
  , _pTry     :: Map Atom (Set Atom) -- ^ What we are trying to prove
  , _pLeadsTo :: Map Atom (Set Atom) -- ^ What we can deduce
  , _pIsBT    :: Bool                -- ^ If under backtracking
  }

makeLenses ''PState

newPState :: PState
newPState = PState 1 S.empty M.empty M.empty False

-- | Pretty print the solution.
prettyPrintSolution :: Solution -> String
prettyPrintSolution = pShow

-- | Given the @Program@ and the Prolog query, return a list of variable
-- substitutions and all intermediate substitutions, each representing a
-- solution.
solve :: Program -> PQuery -> [Solution]
solve p q = runIdentity $ solveS pure (pure ()) pure p q

-- | Similar to @solve@, but prints out each step.
solveIO :: Program -> PQuery -> IO [Solution]
solveIO = solveS onNewStep onFail onBacktractEnd
  where
    untagged    = renamePQuery untag
    onNewStep q = do
      steps <- gets _pStep
      lift $ putStrLn (concat ["Step ", show steps, ":"])
      lift $ case _pqAtoms q of
        [] -> putStrLn "Unification succeeded.\n"
        _  -> putStrLn (concat ["Current query: ", pShow (untagged q), "\n"])
    onFail      = do
      steps <- gets _pStep
      lift $ putStrLn (concat ["Step ", show steps, ":"])
      lift $ putStrLn "Unification failed.\n"
    onBacktractEnd q = lift
      $ putStrLn (concat["Backtracked to the query ", pShow (untagged q), "\n"])

-- | The main function of the Prolog solver.
solveS :: Monad m => (SolvePQuery -> StateT PState m a) -> StateT PState m b
       -> (SolvePQuery -> StateT PState m a) -> Program -> PQuery
       -> m [Solution]
solveS onNewStep onFail onBacktrackEnd (Program _ _ _ cs) pquery
  = fmap (map (optimiseSub . findVarSub))
         (evalStateT (worker M.empty query') newPState)
  where
    cs'                 = renameClause (Nothing ,) <$> cs
    PQuery vars' query' = renamePQuery (Nothing ,) pquery
    subVar subs v       = (v, foldl (flip substituteTerm) (VariableTerm v) subs)
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
        optimisedSubs         = filter (\(v, term) -> VariableTerm v /= term)
                              $ bimap snd (renameTerm snd . substituteTerm validReps) <$> subs
        validReps             = foldr reducer M.empty subs

        reducer (iv@(Nothing, _), term) cur = case term of
          VariableTerm iv'@(Just _, _) -> M.insert iv' (VariableTerm iv) cur
          _                            -> cur
        reducer _ cur                       = cur

    worker sub []         = do
      void $ onNewStep (PQuery vars' [])
      modify ((pIsBT .~ True) . (pStep +~ 1))
      return [[sub]]
    worker sub q@(t : ts) = do
      result <- fmap concat . forM cs' $ \(mH :?<- b) -> case mH of
        Nothing -> pure []
        Just h  -> do
          step <- gets _pStep
          isBT <- gets _pIsBT
          when isBT $ do
            void $ onBacktrackEnd (PQuery vars' q)
            modify (pIsBT .~ False)
          let rename = renameAtom (first (const $ Just step)) -- TODO
          case unifyAtom t (rename h) of
            Nothing   -> pure []
            Just sub' -> do
              onNewStep (PQuery vars' q) >> modify (pStep +~ 1)
              let nextQuery = substituteAtom sub' <$> (map rename b ++ ts)
              rest <- worker sub' nextQuery
              return $ (sub :) <$> rest
      when (null result) $ do
        onFail >> modify (pStep +~ 1)
        modify (pIsBT .~ True)
      return result
