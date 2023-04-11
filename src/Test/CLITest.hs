import           Control.Exception
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
import           Utility.Exception

main :: IO ()
main = runTestTTAndExit
     $ TestList [ TestCase $ assertCLI [] ]


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Given the inputs and the expected @CLIError@s, run the CLI and check if the
-- errors match.
assertCLI :: [(String, Maybe CLIError)] -> Assertion
assertCLI inputsAndexpErrs = void $ do
  let inputs                = fst <$> inputsAndexpErrs -- The user inputs
  -- The expected errors as an array. The first element is @Nothing@ because
  -- The callback is invoked before any input is processed.
  let exrErrArr             = listArray (0, length inputsAndexpErrs)
                                        (Nothing : map snd inputsAndexpErrs)
  -- Check if the error produced at each iteration matches the expected error.
  let testCallback cliState = do
        let curStep = _cliIteration cliState
        let curErr  = _cliErr cliState
        assertEqual ("Match error at step " ++ show curStep)
                    curErr (exrErrArr ! curStep)
  -- Check if the iteration count matches the number of inputs.
  let finalCheck e          = case e of
        Left err                   -> throw err
        Right tErr@(TaggedErr s _) -> do
          liftIO $ assertEqual "Match iteration count"
                               (length inputsAndexpErrs) (_cliIteration s)
          return $ Left tErr
  -- Provide the inputs to the CLI as @stdin@. We add the ":q" command at the
  -- end to quit the program.
  withStdin (unlines inputs ++ "\n:q") . runExceptT $ do
    liftIO $ putStrLn "Welcome to Hrolog!"
    runStateT (dealWithErr finalCheck $ feedbackloop testCallback) initCLIState

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
