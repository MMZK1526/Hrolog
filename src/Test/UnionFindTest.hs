import           Data.List
import           Test.HUnit

import           Utility.UnionFind

main :: IO ()
main = runTestTTAndExit
     $ TestList [ testInitialisedUnionFind
                , testNestedUnification ]

-- | Test if the initialised @UnionFind@ has every element as a singleton set.
testInitialisedUnionFind :: Test
testInitialisedUnionFind = TestLabel "Test initialised UnionFind" . TestCase
                         $ assertEqualList "Initialised UnionFind should have every element as a singleton set"
                                           [0..4]
                                           (fst $ ufFinds [0..4] (iterate ufAdd mkUnionFind !! 5))

-- | Test nested unification.
testNestedUnification :: Test
testNestedUnification = TestLabel "Test nested unification" . TestCase
                      $ assertEqualList "Nested unification should work"
                                        (replicate 8 0)
                                        (fst $ ufFinds [0..7] uf')
  where
    uf  = iterate ufAdd mkUnionFind !! 8
    uf' =  ufUnion 1 5 $ ufUnion 5 6 $ ufUnion 0 3 $ ufUnion 6 7 $ ufUnion 4 5
                       $ ufUnion 2 3 $ ufUnion 0 1 uf


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Assert that two lists are equal, ignoring the order of elements.
assertEqualList :: (Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertEqualList msg expected actual
  = assertEqual msg (sort expected) (sort actual)
