{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains the data types used by the CLI (Main.hs), including
-- @CLIError@, @InputType@, and @CLIState@.
module Internal.CLI.Type where

import           Control.Exception
import           Control.Lens
import           System.IO.Error

import           Program
import           Utility.Exception

-- | A custom error type. It treats @IOError@s as benign errors, printing
-- them out differently based on whether it is a @DoesNotExistError@ or not.
-- It treats @UserInterrupt@ as a serious error because it results in the
-- termination of the program.
--
-- For any other types of errors, it treats them as fatal errors.
data CLIError = DNEError (Maybe String) -- ^ The file does not exist
              | IOException String      -- ^ An IO error
              | UserInter               -- ^ User-induced termination
  deriving (Eq, Show)

-- | The input types of the CLI.
data InputType = InputTypeFilePath FilePath -- ^ Load a program from a file
               | InputTypePQuery PQuery -- ^ Enter a query
               | InputTypeReload -- ^ Reload the program
               | InputHelp -- ^ Print out the help message
               | InputTypeQuit -- ^ Quit the program
               | InputTypeCrash -- ^ Crash the program; for testing purposes
  deriving (Eq, Show)

-- | The state of the CLI.
data CLIState = CLIState { _cliSfilePath :: Maybe FilePath
                         , _cliProgram   :: Maybe Program
                         , _cliPQuery    :: Maybe PQuery
                         , _cliInput     :: Maybe String
                         , _cliErr       :: Maybe (TaggedError CLIError)
                         , _cliIteration :: Int }
makeLenses ''CLIState

-- | The initial state of the CLI.
initCLIState :: CLIState
initCLIState = CLIState Nothing Nothing Nothing Nothing Nothing 0

instance FromSomeError CLIError where
  fromSomeError :: SomeException -> Maybe CLIError
  fromSomeError e = case fromException e :: Maybe IOException of
    Just ioe -> Just $ if isDoesNotExistError ioe
      then DNEError (ioeGetFileName ioe)
      else IOException (show ioe)
    Nothing  ->
      if Just UserInterrupt == (fromException e :: Maybe AsyncException)
        then Just UserInter
        else Nothing

instance HasSeverity CLIError where
  isSerious :: CLIError -> Bool
  isSerious = (UserInter ==)
