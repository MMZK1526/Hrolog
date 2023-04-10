{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Main where

import           Control.Exception
import           Control.Lens
import           System.IO.Error

import           Program
import           Utility.Exception

-- | A custum error type. It treats @IOError@s as non-fatal errors, printing
-- them out differently based on whether it is a @DoesNotExistError@ or not.
--
-- For any other types of errors, it treats them as fatal errors.
--
-- Note that despite @UserInter@ is a fatal error, it is usually treated
-- differently from other fatal errors.
data CLIError = DNEError (Maybe FilePath)
              | IOException String
              | UserInter
              | FatalError String

-- | The input types of the CLI.
data InputType = InputTypeFilePath FilePath
               | InputTypePQuery PQuery
               | InputTypeReload
               | InputHelp
               | InputTypeQuit

-- | The state of the CLI.
data CLIState = CLIState { _cliSfilePath :: Maybe FilePath
                         , _cliProgram   :: Maybe Program
                         , _cliPQuery    :: Maybe PQuery
                         , _cliInput     :: Maybe String
                         , _cliErr       :: Maybe CLIError }
makeLenses ''CLIState

instance FromError CLIError where
  fromError :: SomeException -> CLIError
  fromError e = case fromException e :: Maybe IOException of
    Just ioe -> if isDoesNotExistError ioe
      then DNEError (ioeGetFileName ioe)
      else IOException (show ioe)
    Nothing  ->
      if Just UserInterrupt == (fromException e :: Maybe AsyncException)
        then UserInter
        else FatalError (show e)

  isFatal :: CLIError -> Bool
  isFatal FatalError {} = True
  isFatal UserInter     = True
  isFatal _             = False

