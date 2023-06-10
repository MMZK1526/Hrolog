{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Stack

import           Hrolog.Parser
import           Internal.Program
import           Hrolog.Solver.Deduction
import           Test.HUnit
import           Test.HUnit.Lang

main :: IO ()
main = runTestTTAndExit
     $ TestList [ canPickUpFacts
                , cannotPickUpWrongFacts
                , simpleNumberTestOne
                , simpleNumberTestAll
                , aGTaIsFalse
                , arithmeticTest
                , listTest
                , negTest ]

-- | Can pick up facts.
canPickUpFacts :: Test
canPickUpFacts = TestLabel "Can pick up facts" $ TestList (worker <$> queries)
  where
    worker q = TestCase $ assertSolutionFromFile "./src/Test/programs/facts.hrolog"
                                                 q (Just . Solution $ M.empty)
    queries  = [ mkPQuery ["a"]
               , mkPQuery [mkAtom "b" ["A"]]
               , mkPQuery [mkAtom "c" ["X", "Y"]] ]

-- | Does not derive false facts.
cannotPickUpWrongFacts :: Test
cannotPickUpWrongFacts = TestLabel "Cannot pick up wrong facts" $ TestList (worker <$> queries)
  where
    worker q = TestCase $ assertSolutionFromFile "./src/Test/programs/facts.hrolog"
                                                 q Nothing
    queries  = [ mkPQuery ["d"]
               , mkPQuery [mkAtom "a" ["A", "B"]] ]

-- | A simple test with basic facts and rules, testing for the first solution.
simpleNumberTestOne :: Test
simpleNumberTestOne = TestLabel "Simple number test" . TestCase $
  assertSolutionFromFile "./src/Test/programs/simpleNumbers.hrolog"
                          (mkPQuery [ mkAtom "gt" ["X", "Y"]
                                    , mkAtom "gt" ["Y", "Z"] ])
                          ( Just . Solution $ M.fromList [ ("X", Constant "2")
                                                         , ("Y", Constant "1")
                                                         , ("Z", Constant "0") ] )

-- | A simple test with basic facts and rules, testing for all solutions.
simpleNumberTestAll :: Test
simpleNumberTestAll = TestLabel "Simple number test" . TestCase $
  assertSolutionsFromFile "./src/Test/programs/simpleNumbers.hrolog"
                          (mkPQuery [ mkAtom "gt" ["X", "Y"]
                                    , mkAtom "gt" ["Y", "Z"] ])
                          [ Solution $ M.fromList [ ("X", Constant "2")
                                                  , ("Y", Constant "1")
                                                  , ("Z", Constant "0") ]
                          , Solution $ M.fromList [ ("X", Constant "3")
                                                  , ("Y", Constant "1")
                                                  , ("Z", Constant "0") ]
                          , Solution $ M.fromList [ ("X", Constant "3")
                                                  , ("Y", Constant "2")
                                                  , ("Z", Constant "0") ]
                          , Solution $ M.fromList [ ("X", Constant "3")
                                                  , ("Y", Constant "2")
                                                  , ("Z", Constant "1") ] ]

-- | Test that a > a is False.
aGTaIsFalse :: Test
aGTaIsFalse = TestLabel "a > a is false" . TestCase $
  assertSolutionFromFile "./src/Test/programs/simpleNumbers.hrolog"
                          (mkPQuery [mkAtom "gt" ["X", "X"]])
                          Nothing

-- | Arithmetic test.
arithmeticTest :: Test
arithmeticTest = TestLabel "Arithmetic test" . TestList $
  [ TestLabel "Add tests" $ TestList addTests
  , TestLabel "Sub tests" $ TestList subTests
  , TestLabel "Neg tests" $ TestList neTests
  ]
  where
    addTest x y z
      = assertSolutionFromFile "./src/Test/programs/peanoNumbers.hrolog"
                               (mkPQuery [mkAtom "add" [x, y, "Z"]])
                               (Just . Solution $ M.fromList [("Z", z)])
    addTests = do
      x <- [0..5]
      y <- [0..5]
      let z = x + y
      pure . TestLabel (concat [show x, " + ", show y, " = ", show z])
           . TestCase $ addTest (nats !! x) (nats !! y) (nats !! z)

    subTest x y Nothing
      = assertSolutionFromFile "./src/Test/programs/peanoNumbers.hrolog"
                               (mkPQuery [mkAtom "sub" [x, y, "Z"]])
                               Nothing
    subTest x y (Just z)
      = assertSolutionFromFile "./src/Test/programs/peanoNumbers.hrolog"
                               (mkPQuery [mkAtom "sub"[x, y, "Z"]])
                               (Just . Solution $ M.fromList [("Z", z)])
    subTests = do
      x <- [0..5]
      y <- [0..5]
      let z = x - y
      pure . TestLabel (concat [show x, " - ", show y, " = ", show z])
           . TestCase $ subTest (nats !! x) (nats !! y) (if z >= 0 then Just (nats !! z) else Nothing)

    neTest x y True
      = assertSolutionFromFile "./src/Test/programs/peanoNumbers.hrolog"
                               (mkPQuery [mkAtom "ne" [x, y]])
                               (Just $ Solution M.empty)
    neTest x y False
      = assertSolutionFromFile "./src/Test/programs/peanoNumbers.hrolog"
                               (mkPQuery [mkAtom "ne" [x, y]])
                               Nothing
    neTests = do
      x <- [0..5]
      y <- [0..5]
      pure . TestLabel (concat [show x, " /= ", show y, " = ", show (x /= y)])
           . TestCase $ neTest (nats !! x) (nats !! y) (x /= y)

    zero   = Constant "0"
    sukk x = F (Function "s" 1) [x]
    nats   = iterate sukk zero

-- | List test.
listTest :: Test
listTest = TestLabel "List test" . TestList $
  [ TestLabel "List reverse" $ TestList reverseTests ]
  where
    reverseTest xs xs'
      = assertSolutionFromFile "./src/Test/programs/list.hrolog"
                               (mkPQuery [mkAtom "reverse" [xs, "Y"]])
                               (Just . Solution $ M.fromList [("Y", xs')])
    reverseTests = do
      x <- [0..5 :: Int]
      let xs  = makeList [0..x]
      let xs' = makeList (reverse [0..x])
      pure . TestLabel (concat ["reverse ", show xs, " = ", show xs'])
           . TestCase $ reverseTest xs xs'

    makeList = foldr (\x xs -> F (Function "cons" 2) [Constant (T.pack $ show x), xs]) "nil"

-- | Simple tests on negations
negTest :: Test
negTest = TestLabel "Negation test" . TestCase $ do
  assertSolutionFromFile "./src/Test/programs/simpleNegation.hrolog"
                         (mkPQuery ["a"])
                         (Just . Solution $ M.empty)
  assertSolutionFromFile "./src/Test/programs/simpleNegation.hrolog"
                         (mkPQuery ["x"])
                         (Just . Solution $ M.empty)
  assertSolutionFromFile "./src/Test/programs/simpleNegation.hrolog"
                         (mkPQuery ["!a"])
                         Nothing
  assertSolutionFromFile "./src/Test/programs/simpleNegation.hrolog"
                         (mkPQuery ["!x"])
                         Nothing


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Read the given file and run the given test on the resulting @Program@, see
-- if the first @Solution@ matches.
assertSolutionFromFile :: HasCallStack => FilePath -> PQuery -> Maybe Solution -> Assertion
assertSolutionFromFile filePath q expSol
  = assertFromFile filePath (\p -> assertSolution p q expSol)

-- | Read the given file and run the given test on the resulting @Program@, see
-- if the @Solution@s match.
assertSolutionsFromFile :: HasCallStack => FilePath -> PQuery -> [Solution] -> Assertion
assertSolutionsFromFile filePath q expSols
  = assertFromFile filePath (\p -> assertSolutions p q expSols)

-- | Handle any @Exception@ by simply recording them as @String@s.
handleErr :: ExceptT Text IO () -> ExceptT Text IO ()
handleErr = mapExceptT (`catches` [Handler rethrowTestFailure, Handler logOtherErrors])
  where
    rethrowTestFailure (e :: HUnitFailure) = throw e
    logOtherErrors (e :: SomeException)    = pure (Left ("Exception: "<> T.pack (show e)))

-- | Use the given @Assertion@ which takes a @Program@. The @Program@ comes from
-- reading the given file.
assertFromFile :: HasCallStack => FilePath -> (Program -> Assertion) -> Assertion
assertFromFile filePath testFun = do
  result <- runExceptT . handleErr $ do
    p <- lift (parseProgram <$> T.readFile filePath) >>= ExceptT . pure
    lift $ testFun p
  case result of
    Left err -> assertFailure $ T.unpack err
    Right () -> pure ()

-- | Assert that the given @Program@ and @PQuery@ produce the expected first
-- @Solution@.
assertSolution :: HasCallStack => Program -> PQuery -> Maybe Solution -> Assertion
assertSolution p q expSol
  = assertEqual "solution" expSol (solveOne p q)

-- | Assert that the given @Program@ and @PQuery@ produce the expected
-- @Solution@s.
assertSolutions :: HasCallStack => Program -> PQuery -> [Solution] -> Assertion
assertSolutions p q expSols
  = assertEqual "solutions" (sort expSols) (sort (solve p q))
