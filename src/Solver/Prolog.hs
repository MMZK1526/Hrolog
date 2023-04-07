{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Solver.Prolog where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Internal.Program
import           Utility.PP
import           Utility.Unifiers

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
solve :: Program -> PQuery -> [(Solution, [Map (Maybe Int, String) (Term' (Maybe Int, String))])]
solve p q = runIdentity $ solveS pure (pure ()) pure p q

-- -- | Similar to @solve@, but prints out each step.
-- solveIO :: Program -> PQuery -> IO [(Solution, [Map String Term])]
-- solveIO = solveS onNewStep onFail onBacktractEnd
--   where
--     onNewStep q = do
--       steps <- gets _pStep
--       lift $ putStrLn (concat ["Step ", show steps, ":"])
--       case _pqAtoms q of
--         [] -> lift $ putStrLn "Unification succeeded.\n"
--         _  -> lift $ putStrLn (concat ["Current query: ", pShow q, "\n"])
--     onFail      = do
--       steps <- gets _pStep
--       lift $ putStrLn (concat ["Step ", show steps, ":"])
--       lift $ putStrLn "Unification failed.\n"
--     onBacktractEnd q
--       = lift $ putStrLn (concat["Backtracked to the query ", pShow q, "\n"])

-- | The main function of the Prolog solver.
solveS :: Monad m => (SolvePQuery -> StateT PState m a) -> StateT PState m b
       -> (SolvePQuery -> StateT PState m a) -> Program -> PQuery
       -> m [(Solution, [Map (Maybe Int, String) (Term' (Maybe Int, String))])]
solveS onNewStep onFail onBacktrackEnd (Program _ _ _ cs) pquery
  = fmap (map (\subs -> (optimiseSub $ findVarSub subs, subs)))
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
