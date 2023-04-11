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

-- | A custum error type. It treats @IOError@s as benign errors, printing
-- them out differently based on whether it is a @DoesNotExistError@ or not.
-- It treats @UserInterrupt@ as a serious error because it results in the
-- termination of the program.
--
-- For any other types of errors, it treats them as fatal errors.
data CLIError = DNEError (Maybe FilePath)
              | IOException String
              | UserInter
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
                         , _cliErr       :: Maybe CLIError
                         , _cliIteration :: Int }
makeLenses ''CLIState

-- | The initial state of the CLI.
initCLIState :: CLIState
initCLIState = CLIState Nothing Nothing Nothing Nothing Nothing 0

instance FromError CLIError where
  fromError :: SomeException -> Either SomeException CLIError
  fromError e = case fromException e :: Maybe IOException of
    Just ioe -> if isDoesNotExistError ioe
      then Right $ DNEError (ioeGetFileName ioe)
      else Right $ IOException (show ioe)
    Nothing  ->
      if Just UserInterrupt == (fromException e :: Maybe AsyncException)
        then Right UserInter
        else Left e

instance HasSeverity CLIError where
  isSerious :: CLIError -> Bool
  isSerious UserInter = True
  isSerious _         = False
