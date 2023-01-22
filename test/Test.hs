import           Parser
import           Program
import           Test.HUnit

main :: IO ()
main = runTestTTAndExit
     $ TestList [ canParseVariable ]

canParseVariable :: Test
canParseVariable
  = TestLabel "Can parse variable" . TestCase $ do
      assertValid "Empty string" Nothing (parse variable "")
      assertValid "Uppercase letter" (Just "A") (parse variable "A")
      assertValid "Uppercase word" (Just "Abc") (parse variable "Abc")
      assertValid "Lowercase" Nothing (parse variable "abc")
      assertValid "Underscore" Nothing (parse variable "_")

assertValid :: (Eq a, Show a) => String -> Maybe a -> Either e a -> Assertion
assertValid str Nothing act = case act of
    Left _  -> pure ()
    Right _ -> assertFailure str
assertValid str (Just a) act = case act of
    Left _   -> assertFailure str
    Right a' -> assertEqual str a a'
