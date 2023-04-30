{-# LANGUAGE TypeApplications #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.IORef
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
     $ TestList [ testEmptyInput
                , testReloadBeforeLoad ]

-- | Test if the CLI works with no input (immediate quit).
testEmptyInput :: Test
testEmptyInput = TestLabel "Test empty input" . TestCase $ assertCLI []

-- | When the user reloads before loading a program, there should be no program.
testReloadBeforeLoad :: Test
testReloadBeforeLoad = TestLabel "Test reload before load" . TestCase
                     $ assertCLI [ (":r", CLISnapshot Nothing Nothing) ]


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | @CLIError@ but only the constructors.
data CLIErrorType = DNEErrT
                  | IOErrT
                  | UserInterT
                  | InternalErrT
                  | FatalErrT
  deriving (Eq, Show)

-- | Convert a @TaggedError CLIError@ to a @CLIErrorType@.
cliErrorToType :: TaggedError CLIError -> CLIErrorType
cliErrorToType (TaggedError _ Nothing)    = FatalErrT
cliErrorToType (TaggedError _ (Just err)) = case err of
  DNEErr _      -> DNEErrT
  IOErr _       -> IOErrT
  UserInter     -> UserInterT
  InternalErr _ -> InternalErrT

-- | A "snapshot" of the @CLIState@ that is used for testing.
data CLISnapshot = CLISnapshot { sfilePath :: Maybe FilePath
                               , sErrType  :: Maybe CLIErrorType }
  deriving (Eq, Show)

-- | Take a "snapshot" of the @CLIState@.
takeSnapshot :: CLIState -> CLISnapshot
takeSnapshot s = CLISnapshot (_cliFilePath s) (cliErrorToType <$> _cliErr s)

-- | The initial @Snapshot@ corresponding to the initial @CLIState@.
initialSnapshot :: CLISnapshot
initialSnapshot = takeSnapshot initCLIState

-- | Given the inputs and the expected @CLIError@s, run the CLI and check if the
-- snapshots match.
assertCLI :: [(String, CLISnapshot)] -> Assertion
assertCLI inputsAndsnapshots = void $ do
  curStepRef <- newIORef 0
  let inputs       = fst <$> inputsAndsnapshots -- The user inputs
  -- The expected snapshots as an array. We add an initial snapshot because
  -- the callback is first invoked without any input.
  let snapshotsArr = listArray (0, length inputsAndsnapshots)
                              (initialSnapshot : map snd inputsAndsnapshots)
  -- Check if the snapshot produced at each iteration matches the corresponding
  -- expected snapshot.
  let testCallback cliState = do
        let curStep     = _cliIteration cliState
        let curSnapshot = takeSnapshot cliState
        assertEqual ("Match snapshot at step " ++ show curStep)
                    curSnapshot (snapshotsArr ! curStep)
        modifyIORef' curStepRef succ -- Increment the current step
  -- Check if the iteration count matches the number of inputs.
  let finalCheck            = const $ do
        stepCount <- liftIO $ readIORef curStepRef
        -- We add 1 to the length of the inputs because it does not include the
        -- last ":q" command.
        liftIO $ assertEqual "Match iteration count"
                             (length inputsAndsnapshots + 1) stepCount
        pure $ Right ()
  -- Provide the inputs to the CLI as @stdin@. We add the ":q" command at the
  -- end to quit the program.
  withStdin (unlines inputs ++ "\n:q") . runExceptT $ do
    liftIO $ putStrLn "Welcome to Hrolog!"
    runStateT (dealWithErr @(TaggedError CLIError) finalCheck $ feedbackloop testCallback) initCLIState

-- | Replace the @stdin@ of an @IO@ action with a @String@ for an IO action,
-- getting back the old @stdin@ at the end.
withStdin :: String -> IO a -> IO a
withStdin input action = do
  oldStdin <- hDuplicate stdin
  newStdin <- stringInput input
  hDuplicateTo newStdin stdin
  result <- action
  hDuplicateTo oldStdin stdin
  pure result

-- | Turn a @String@ into a read-end @Handle@.
stringInput :: String -> IO Handle
stringInput input = do
  (r, w) <- createPipe
  hPutStr w input
  hClose w
  pure r
