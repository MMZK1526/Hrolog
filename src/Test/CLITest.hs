import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           GHC.Arr
import           GHC.IO.Handle
import           Test.HUnit
import           System.IO
import           System.Process

import           Internal.CLI
import           Internal.CLI.Type

main :: IO ()
main = runTestTTAndExit
     $ TestList [ {- TestCase $ assertCLI [] -} ]


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

assertCLI :: [(String, Maybe CLIError)] -> Assertion
assertCLI inputsAndexpErrs = do
  let inputs    = fst <$> inputsAndexpErrs
  let exrErrArr = listArray (0, length inputsAndexpErrs)
                            (snd <$> inputsAndexpErrs)
  result <- withStdin (unlines inputs) $ runExceptT . void $ do
    liftIO $ putStrLn "Welcome to Hrolog!"
    runStateT (feedbackloop (const $ assertFailure "FOO")) initCLIState
  case result of
    Left err -> errHandler err
    Right _  -> pure ()
  undefined

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
