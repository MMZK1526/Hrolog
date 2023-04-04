
import           Program
import           Solver.Prolog
import           Test.HUnit

main :: IO ()
main = runTestTTAndExit
     $ TestList []

testSolver :: String -> Program -> PQuery -> [Solution] -> Test
testSolver lbl p q expSols = TestLabel lbl . TestCase $ do
  assertEqual "solutions" expSols (fst <$> solve p q)
