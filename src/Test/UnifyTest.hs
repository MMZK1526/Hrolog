{-# LANGUAGE OverloadedStrings #-}

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
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
                , cannotUnifyVariableWithVariableBoundToDifferentConstants
                , canUnifyVariableWithFunctionTerm
                , cannotUnifyDifferentFunctionNames
                , cannotUnifyVariableWithFunctionTermUsingSelfAsArgument
                , cannotUnifyRecursiveFunctionTerms
                , canUnifyFunctionsWithUnifiableShapes
                , anyAtomSubsumesItself
                , functionsCannotSubsumeVariables
                , variablesSubsumeEverything
                , variablesCannotSubsumeAnyLongerIfBoundToConstant ]

-- | Can unify the same 0-ary predicate.
canUnifySameLiteral :: Test
canUnifySameLiteral = TestLabel "Can unify same 0-ary predicate" $ TestCase $
  assertUnifyAtom (mkAtom "a" []) (mkAtom "a" []) (Just M.empty)

-- | Cannot unify 0-ary predicates that are not the same.
cannotUnifyDifferentLiterals :: Test
cannotUnifyDifferentLiterals = TestLabel "Cannot unify different 0-ary predicates" $ TestCase $
  assertUnifyAtom (mkAtom "a" []) (mkAtom "b" []) Nothing

-- | Cannot unify predicates with different arities.
cannotUnifyDifferentArity :: Test
cannotUnifyDifferentArity = TestLabel "Cannot unify predicates with different arities" $ TestCase $
  assertUnifyAtom (mkAtom "a" []) (mkAtom "a" ["A"]) Nothing

-- | Can unify predicates with the same constants.
canUnifyPredicatesWithSameConstants :: Test
canUnifyPredicatesWithSameConstants = TestLabel "Can unify predicates with the same constants" $ TestCase $
  assertUnifyAtom (mkAtom "a" [Constant "1", Constant "2"])
                  (mkAtom "a" [Constant "1", Constant "2"])
                  (Just M.empty)

-- | Cannot unify predicates with different constants.
cannotUnifyDifferentConstants :: Test
cannotUnifyDifferentConstants = TestLabel "Cannot unify predicates with different constants" $ TestCase $
  assertUnifyAtom (mkAtom "a" [Constant "1", Constant "2"])
                  (mkAtom "a" [Constant "2", Constant "1"])
                  Nothing

-- | Can unify variable with constant.
canUnifyVariableWithConstant :: Test
canUnifyVariableWithConstant = TestLabel "Can unify variable with constant" $ TestCase $
  assertUnifyAtom (mkAtom "a" [Constant "1", "A"])
                  (mkAtom "a" [Constant "1", Constant "2"])
                  (Just $ M.fromList [("A", Constant "2")])

-- | Can unify variable with another variable.
canUnifyVariableWithVariable :: Test
canUnifyVariableWithVariable = TestLabel "Can unify variable with variable" $ TestCase $
  assertUnifyAtom (mkAtom "a" ["A"])
                  (mkAtom "a" ["B"])
                  (Just $ M.fromList [("A", "A"), ("B", "A")])

-- | Cannot unify a variable with a constant if the variable has already been
--  bound to a different constant.
cannotUnifyVariableWithNewConstant :: Test
cannotUnifyVariableWithNewConstant = TestLabel "Cannot unify variable with new constant" $ TestCase $
  assertUnifyAtom (mkAtom "a" ["A", "A"])
                  (mkAtom "a" [Constant "1", Constant "2"])
                  Nothing

-- | If a variable is bound to a constant, all other variables that bind to the
-- same variable should also be bound to the same constant.
canUnifyVariableWithVariableBoundToConstant :: Test
canUnifyVariableWithVariableBoundToConstant = TestLabel "Can unify variable with variable bound to constant" $ TestCase $
  assertUnifyAtom (mkAtom "a" ["A", "B"])
                  (mkAtom "a" [Constant "1", "A"])
                  (Just $ M.fromList [("A", Constant "1"), ("B", Constant "1")])

-- | If two variables are bound to different constants, they cannot be unified.
cannotUnifyVariableWithVariableBoundToDifferentConstants :: Test
cannotUnifyVariableWithVariableBoundToDifferentConstants = TestLabel "Cannot unify variable with variable bound to different constants" $ TestCase $
  assertUnifyAtom (mkAtom "a" ["A", "B", "A"])
                  (mkAtom "a" [Constant "1", Constant "2", "B"])
                  Nothing

-- | Can unify a variable with a function term.
canUnifyVariableWithFunctionTerm :: Test
canUnifyVariableWithFunctionTerm = TestLabel "Can unify variable with function term" $ TestCase $
  assertUnifyAtom (mkAtom "a" ["A", "B"])
                  (mkAtom "a" [mkFTerm' "f" [Constant "1"], "A"])
                  (Just $ M.fromList [("A", mkFTerm' "f" [Constant "1"]), ("B", mkFTerm' "f" [Constant "1"])])

-- | Cannot unify functions with different names.
cannotUnifyDifferentFunctionNames :: Test
cannotUnifyDifferentFunctionNames = TestLabel "Cannot unify functions with different names" $ TestCase $
  assertUnifyAtom (mkAtom "a" [mkFTerm' "f" [Constant "1"], "A"])
                  (mkAtom "a" [mkFTerm' "g" [Constant "1"], "A"])
                  Nothing

-- | Cannot unify a variable with a function term that uses itself as an
-- argument.
cannotUnifyVariableWithFunctionTermUsingSelfAsArgument :: Test
cannotUnifyVariableWithFunctionTermUsingSelfAsArgument = TestLabel "Cannot unify variable with function term using self as argument" $ TestCase $
  assertUnifyAtom (mkAtom "a" ["A"])
                  (mkAtom "a" [mkFTerm' "f" ["A"]])
                  Nothing

-- | Cannot unify function terms that refers each other recursively.
cannotUnifyRecursiveFunctionTerms :: Test
cannotUnifyRecursiveFunctionTerms = TestLabel "Cannot unify recursive function terms" $ TestCase $
  assertUnifyAtom (mkAtom "a" ["A", "B"])
                  (mkAtom "a" [mkFTerm' "f" ["B"], mkFTerm' "f" ["A"]])
                  Nothing

-- | Can unify functions with functions if they have unifiable shapes.
canUnifyFunctionsWithUnifiableShapes :: Test
canUnifyFunctionsWithUnifiableShapes = TestLabel "Can unify functions with unifiable shapes" $ TestCase $
  assertUnifyAtom (mkAtom "a" [mkFTerm' "f" ["A"]])
                  (mkAtom "a" [mkFTerm' "f" [F (Function "g" 2) [Constant "1", "C"]]])
                  (Just $ M.fromList [("A", F (Function "g" 2) [Constant "1", "C"]), ("C","C")])

-- | Any atom subsumes itself.
anyAtomSubsumesItself :: Test
anyAtomSubsumesItself = TestLabel "Any atom subsumes itself" $ TestCase $ do
  assertSubsumesAtom (mkAtom "a" ["A"]) (mkAtom "a" ["A"])
  assertSubsumesAtom (mkAtom "a" [Constant "1"]) (mkAtom "a" [Constant "1"])
  assertSubsumesAtom (mkAtom "a" [mkFTerm' "f" ["A"]]) (mkAtom "a" [mkFTerm' "f" ["A"]])

-- | Function terms cannot subsume variables.
functionsCannotSubsumeVariables :: Test
functionsCannotSubsumeVariables = TestLabel "Functions cannot subsume variables" $ TestCase $
  assertNotSubsumeAtom (mkAtom "a" [mkFTerm' "f" ["A"]]) (mkAtom "a" ["B"])

-- | Variables subsume everything.
variablesSubsumeEverything :: Test
variablesSubsumeEverything = TestLabel "Variables subsume everything" $ TestCase $ do
  assertSubsumesAtom (mkAtom "a" ["A"]) (mkAtom "a" ["B"])
  assertSubsumesAtom (mkAtom "a" ["A"]) (mkAtom "a" [Constant "1"])
  assertSubsumesAtom (mkAtom "a" ["A"]) (mkAtom "a" [mkFTerm' "f" ["B"]])
  assertSubsumesAtom (mkAtom "a" ["A"]) (mkAtom "a" [F (Function "f" 2) ["B", Constant "1"]])

-- | Variables cannot subsume any longer if they are bound to a constant.
variablesCannotSubsumeAnyLongerIfBoundToConstant :: Test
variablesCannotSubsumeAnyLongerIfBoundToConstant = TestLabel "Variables cannot subsume other things if bound to constant" $ TestCase $ do
  assertNotSubsumeAtom (mkAtom "a" ["A", "A"]) (mkAtom "a" [Constant "1", mkFTerm' "f" ["B"]])
  assertNotSubsumeAtom (mkAtom "a" ["A", "B", "A"]) (mkAtom "a" [Constant "1", Constant "2", "B"])


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

assertUnifyAtom :: HasCallStack => Atom -> Atom -> Maybe (Map Text Term) -> Assertion
assertUnifyAtom a1 a2 expSub = do
  assertEqual "Atom Unification" expSub (unifyAtom a1 a2)

assertSubsumesAtom :: HasCallStack => Atom -> Atom -> Assertion
assertSubsumesAtom a1 a2 = assertBool "Subsumes" (subsumes a1 a2)

assertNotSubsumeAtom :: HasCallStack => Atom -> Atom -> Assertion
assertNotSubsumeAtom a1 a2 = assertBool "Not subsume" (not $ subsumes a1 a2)
