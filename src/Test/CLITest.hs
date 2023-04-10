import           GHC.IO.Handle
import           Internal.CLI.Type
import           System.IO
import           System.Process

main :: IO ()
main = do
  withStdin "Hello Haskell\nMMZK1526" $ do
    x <- getLine
    putStrLn x


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- assertCLI :: [String] -> [CLIError]

-- | Replace the @stdin@ of an @IO@ action with a @String@ for an IO action,
-- getting back the old @stdin@ at the end.
withStdin :: String -> IO a -> IO a
withStdin input action = do
  oldStdin <- hDuplicate stdin
  newStdin <- stringInput input
  hDuplicateTo newStdin stdin
  result <- action
  hDuplicateTo oldStdin stdin
  return result

-- | Turn a @String@ into a read-end @Handle@.
stringInput :: String -> IO Handle
stringInput input = do
  (r, w) <- createPipe
  hPutStr w input
  hClose w
  return r
