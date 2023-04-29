import qualified Data.IntMap.Strict as IM
import           Test.HUnit

import           Utility.Graph

main :: IO ()
main = runTestTTAndExit
     $ TestList [ testEmptyGraph
                , testSingleNodeGraph
                , testSingleLoopGraph
                , testCircularPathGraph
                , testDAG ]

-- | Test if the empty graph contains a cycle.
testEmptyGraph :: Test
testEmptyGraph = TestLabel "Test empty graph" . TestCase
               $ assertBool "Empty graph should not contain a cycle"
                             (not (hasCycle (Graph mempty)))

-- | Test if the graph with a single node contains a cycle.
testSingleNodeGraph :: Test
testSingleNodeGraph = TestLabel "Test single node graph" . TestCase
                    $ assertBool "Single node graph should not contain a cycle"
                                 (not (hasCycle (Graph (IM.singleton 0 []))))

-- | Test if the graph with single loop contains a cycle.
testSingleLoopGraph :: Test
testSingleLoopGraph = TestLabel "Test single loop graph" . TestCase
                    $ assertBool "Single loop graph should contain a cycle"
                                 (hasCycle (Graph (IM.fromList [(0, [0])])))

-- | Test if a graph with a circular path between two nodes contains a cycle.
testCircularPathGraph :: Test
testCircularPathGraph = TestLabel "Test circular path graph" . TestCase
                      $ assertBool "Circular path graph should contain a cycle"
                                   (hasCycle (Graph (IM.fromList [(0, [1]), (1, [0])])))

-- | Test if DAG contains a cycle.
--
-- It is a graph where 0 connects to 1 and 2, 1 and 2 connect to 3, 3 connect to
-- 5, and 2 and 5 connects to 4.
testDAG :: Test
testDAG = TestLabel "Test DAG" . TestCase
        $ assertBool "DAG should not contain a cycle"
                     (not (hasCycle (Graph (IM.fromList [(0, [1, 2]), (1, [3]), (2, [3, 4]), (3, [5]), (4, []), (5, [4])]))))