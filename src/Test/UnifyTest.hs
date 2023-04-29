import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           GHC.Stack
import           Test.HUnit

import           Internal.Program
import           Utility.Unifiers

main :: IO ()
main = runTestTTAndExit
     $ TestList [ canUnifySameLiteral
                , cannotUnifyDifferentLiterals
                , cannotUnifyDifferentArity
                , canUnifyPredicatesWithSameConstants
                , cannotUnifyDifferentConstants
                , canUnifyVariableWithConstant
                , canUnifyVariableWithVariable
                , cannotUnifyVariableWithNewConstant
                , canUnifyVariableWithVariableBoundToConstant
                , cannotUnifyVariableWithVariableBoundToDifferentConstants ]

-- | Can unify the same 0-ary predicate.
canUnifySameLiteral :: Test
canUnifySameLiteral = TestLabel "Can unify same 0-ary predicate" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 0) []) (Atom (Predicate "a" 0) []) (Just M.empty)

-- | Cannot unify 0-ary predicates that are not the same.
cannotUnifyDifferentLiterals :: Test
cannotUnifyDifferentLiterals = TestLabel "Cannot unify different 0-ary predicates" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 0) []) (Atom (Predicate "b" 0) []) Nothing

-- | Cannot unify predicates with different arities.
cannotUnifyDifferentArity :: Test
cannotUnifyDifferentArity = TestLabel "Cannot unify predicates with different arities" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 0) []) (Atom (Predicate "a" 1) [VariableTerm "A"]) Nothing

-- | Can unify predicates with the same constants.
canUnifyPredicatesWithSameConstants :: Test
canUnifyPredicatesWithSameConstants = TestLabel "Can unify predicates with the same constants" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 2) [Constant "1", Constant "2"])
                  (Atom (Predicate "a" 2) [Constant "1", Constant "2"])
                  (Just M.empty)

-- | Cannot unify predicates with different constants.
cannotUnifyDifferentConstants :: Test
cannotUnifyDifferentConstants = TestLabel "Cannot unify predicates with different constants" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 2) [Constant "1", Constant "2"])
                  (Atom (Predicate "a" 2) [Constant "2", Constant "1"])
                  Nothing

-- | Can unify variable with constant.
canUnifyVariableWithConstant :: Test
canUnifyVariableWithConstant = TestLabel "Can unify variable with constant" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 2) [Constant "1", VariableTerm "A"])
                  (Atom (Predicate "a" 2) [Constant "1", Constant "2"])
                  (Just $ M.fromList [("A", Constant "2")])

-- | Can unify variable with another variable.
canUnifyVariableWithVariable :: Test
canUnifyVariableWithVariable = TestLabel "Can unify variable with variable" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 1) [VariableTerm "A"])
                  (Atom (Predicate "a" 1) [VariableTerm "B"])
                  (Just $ M.fromList [("A", VariableTerm "A"), ("B", VariableTerm "A")])

-- | Cannot unify a variable with a constant if the variable has already been
--  bound to a different constant.
cannotUnifyVariableWithNewConstant :: Test
cannotUnifyVariableWithNewConstant = TestLabel "Cannot unify variable with new constant" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 2) [VariableTerm "A", VariableTerm "A"])
                  (Atom (Predicate "a" 2) [Constant "1", Constant "2"])
                  Nothing

-- | If a variable is bound to a constant, all other variables that bind to the
-- same variable should also be bound to the same constant.
canUnifyVariableWithVariableBoundToConstant :: Test
canUnifyVariableWithVariableBoundToConstant = TestLabel "Can unify variable with variable bound to constant" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 2) [VariableTerm "A", VariableTerm "B"])
                  (Atom (Predicate "a" 2) [Constant "1", VariableTerm "A"])
                  (Just $ M.fromList [("A", Constant "1"), ("B", Constant "1")])

-- | If two variables are bound to different constants, they cannot be unified.
cannotUnifyVariableWithVariableBoundToDifferentConstants :: Test
cannotUnifyVariableWithVariableBoundToDifferentConstants = TestLabel "Cannot unify variable with variable bound to different constants" $ TestCase $
  assertUnifyAtom (Atom (Predicate "a" 3) [VariableTerm "A", VariableTerm "B", VariableTerm "A"])
                  (Atom (Predicate "a" 3) [Constant "1", Constant "2", VariableTerm "B"])
                  Nothing


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

assertUnifyAtom :: HasCallStack => Atom -> Atom -> Maybe (Map String Term) -> Assertion
assertUnifyAtom a1 a2 expSub = do
  assertEqual "Atom Unification" expSub (unifyAtom a1 a2)
