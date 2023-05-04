{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains the data types used by the CLI (Main.hs), including
-- @CLIError@, @InputType@, and @CLIState@.
module Internal.CLI.Type where

import           Control.Exception
import           Control.Monad.Trans.Writer.CPS
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO.Error

import           Program
import           Utility.Exception
import           Utility.PP

-- | A custom error type for the CLI.
data CLIError = DNEErr (Maybe String) -- ^ The file does not exist
              | IOErr String -- ^ Any other IO error
              | InternalErr String -- ^ An internal error; should not happen
              | UserInter -- ^ User-induced termination
  deriving (Eq, Show)

-- | The input types of the CLI.
data InputType = InputTypeFilePath FilePath -- ^ Load a program from a file
               | InputTypePQuery PQuery -- ^ Enter a query
               | InputTypeReload -- ^ Reload the program
               | InputTypeHelp -- ^ Print out the help message
               | InputTypeQuit -- ^ Quit the program
  deriving (Eq, Show)

-- | The state of the CLI.
data CLIState = CLIState { _cliFilePath  :: Maybe FilePath
                         , _cliProgram   :: Maybe Program
                         , _cliPQuery    :: Maybe PQuery
                         , _cliInput     :: Maybe Text
                         , _cliErr       :: Maybe (TaggedError CLIError)
                         , _cliIteration :: Int }
makeLenses ''CLIState

instance PP () CLIState where
  pShowF :: () -> CLIState -> Text
  pShowF _ cliState = execWriter $ do
    tell "CLI State:\n"
    case cliState ^. cliFilePath of
      Just fp -> tell $ T.concat ["  File path: ", T.pack fp, "\n"]
      Nothing -> pure ()
    case cliState ^. cliProgram of
      Just p  -> tell $ T.concat ["  Program:\n", pShow p, "\n"]
      Nothing -> pure ()
    case cliState ^. cliPQuery of
      Just pq -> tell $ T.concat ["  Query:\n", pShow pq, "\n"]
      Nothing -> pure ()

-- | The initial state of the CLI.
initCLIState :: CLIState
initCLIState = CLIState Nothing Nothing Nothing Nothing Nothing 0

instance FromSomeError CLIError where
  fromSomeError :: SomeException -> Maybe CLIError
  fromSomeError e = case fromException e :: Maybe IOException of
    Just ioe -> Just $ if 
      | isDoesNotExistError ioe -> DNEErr (ioeGetFileName ioe)
      | isUserError ioe         -> InternalErr $ ioeGetErrorString ioe
      | otherwise               -> IOErr (show ioe)
    Nothing  ->
      if Just UserInterrupt == (fromException e :: Maybe AsyncException)
        then Just UserInter
        else Nothing

instance HasSeverity CLIError where
  isSerious :: CLIError -> Bool
  isSerious = (UserInter ==)
