{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           System.IO.Error
import           System.Directory
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import           Parser
import           Program
import           Solver.Prolog
import           Utility.Exception
import           Utility.Parser

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
                         , _cliInput     :: Maybe String }

makeLenses ''CLIState

main :: IO ()
main = do
  result <- runExceptT . void $ do
    liftIO $ putStrLn "Welcome to Hrolog!"
    runStateT feedbackloop $ CLIState Nothing Nothing Nothing Nothing
  case result of
    Left err -> errHandler err
    Right _  -> pure ()

-- | The CLI loop. It takes an input, parses it, and executes the corresponding,
-- forever.
feedbackloop :: StateT CLIState (ExceptT CLIError IO) ()
-- The "forever" indicates that the loop will never terminate unless there is
-- an uncaught exception.
-- "handleErrS" is a utility function that catches all "IOException"s by
-- printing them out. In other words, if an "IOException" is thrown, the program
-- will ignore the current progress, print out the error, and continue to the
-- next loop.
-- On the other hand, it does not catch other errors (such as user-induced
-- termination). In this case, the function transforms this error into a pure
-- "String" exception wrapped in an "ExceptT", and the program will break from
-- "forever" and terminate with a message corresponding to the content of the
-- "String".
feedbackloop = forever . handleErr'_ $ do
  mProg <- use cliProgram -- Get the program from the state.
  -- Get the user input.
  input <- do
    mInput <- use cliInput
    case mInput of
      -- If the input is already stored in the state, we use it.
      Just input -> input <$ (cliInput .= Nothing)
      -- Otherwise, we prompt the user for input.
      Nothing    -> liftIO getLine'
  -- Parse the input.
  case evalState (parseT space inputP input) mProg of
    -- If the input is invalid, print an error message.
    Left err                            -> liftIO $ do
      putStrLn "Error parsing the input:"
      putStrLn err
    -- A @Nothing@ means that the input is a query, but the program is not
    -- loaded. In this case, we print an error message.
    Right Nothing                       ->
      liftIO $ putStrLn "Cannot take query without a program loaded."
    -- Dispatch the input to the corresponding handler.
    Right (Just (InputTypeFilePath fp)) -> handleNewProgram fp
    Right (Just InputTypeReload)        -> handleReload
    Right (Just (InputTypePQuery q))    -> handlePQuery q
    Right (Just InputHelp)              -> handleHelp
    Right (Just InputTypeQuit)          -> handleQuit


--------------------------------------------------------------------------------
-- Input Parsers & Helpers
--------------------------------------------------------------------------------

-- | Parse an input. The @Program@ in the inner @StateT@ is provided by the
-- "Program" in the @CLIState@.
inputP :: Monad m => ParserT (StateT (Maybe Program) m) (Maybe InputType)
inputP = P.choice [ Just . InputTypeFilePath <$> inputFilePath
                  , Just InputTypeReload <$ inputReload
                  , Just InputTypeQuit <$ inputQuit
                  , Just InputHelp <$ inputHelp
                  , fmap InputTypePQuery <$> pQuery ]

-- | Parse a file path.
inputFilePath :: Monad m => ParserT m FilePath
inputFilePath = string ":l" >> P.many P.anySingle
{-# INLINE inputFilePath #-}

-- | Parse a reload command.
inputReload :: Monad m => ParserT m ()
inputReload = void $ string ":r"
{-# INLINE inputReload #-}

-- | Parse a help command.
inputHelp :: Monad m => ParserT m ()
inputHelp = void $ string ":h"
{-# INLINE inputHelp #-}

-- | Parse a quit command.
inputQuit :: Monad m => ParserT m ()
inputQuit = void $ string ":q"
{-# INLINE inputQuit #-}

-- | Parse spaces.
space :: Monad m => ParserT m ()
space = L.space P.space1 P.empty P.empty

-- | Parse a string with space after it.
string :: Monad m => String -> ParserT m String
string = L.lexeme space . P.string

-- | Repeatedly read input from the user until a non-empty @String@ is read.
getLine' :: IO String
getLine' = do
  input <- getLine
  case parse space (pure ()) input of 
    Left _  -> pure input
    Right _ -> getLine'


--------------------------------------------------------------------------------
-- Input Handlers & Helpers
--------------------------------------------------------------------------------

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

  errHandler :: CLIError -> IO ()
  errHandler (DNEError mfp)  = do
    curDir <- liftIO getCurrentDirectory
    let fileDNEErrMsg = case mfp of
          Just fp -> concat ["File ", show fp, " does not exist."]
          _       -> "File does not exist."
    let curDirMsg     = concat ["Current directory is ", show curDir, "."]
    putStrLn $ unlines [fileDNEErrMsg, curDirMsg]
  errHandler (IOException e) = putStrLn ("IO Error:\n" ++ e)
  errHandler UserInter       = putStrLn "Quit by user."
  errHandler (FatalError e)  = putStrLn ("Fatal Error:\n" ++ e)

-- | Handle loading a program from a path.
handleNewProgram :: MonadIO m => FilePath -> StateT CLIState m ()
handleNewProgram fp = do
  cliSfilePath .= Just fp -- Store the file path in the state.
  newProg <- liftIO $ readFile fp -- Read the program from the file.
  case parseProgram newProg of
    -- If the program is invalid, print an error message.
    Left pErr  -> liftIO $ do
      putStrLn "Error parsing the program!"
      putStrLn pErr
    -- If the program is valid, store it in the state.
    Right prog -> do
      cliProgram .= Just prog
      liftIO $ putStrLn (concat ["Program ", show fp, " loaded:"])
      liftIO $ putStrLn $ prettifyProgram prog

-- | Handle reloading the program from the file path stored in the state.
handleReload :: MonadIO m => StateT CLIState m ()
handleReload = do
  mfp <- use cliSfilePath -- Get the file path from the state.
  case mfp of
    -- If the file path is not stored, let the user know.
    Nothing -> liftIO $ putStrLn "No program loaded."
    -- If the file path is stored, reload the program from the file.
    Just fp -> handleNewProgram fp

-- | Handle parsing and solving with a query.
handlePQuery :: MonadIO m => PQuery -> StateT CLIState m ()
handlePQuery q = do
  mProg <- use cliProgram -- Get the program from the state.
  case mProg of
    -- If the program is not stored, let the user know.
    Nothing -> liftIO $ putStrLn "No program loaded."
    -- If the program is stored, solve the query.
    Just prog -> do
      let handleSolutions []           = liftIO $ putStrLn "No more solutions."
          handleSolutions (sol : sols) = do
            liftIO . putStrLn $ concat 
              [ "\nSolution:\n", prettifySolution sol, "\n"
              , "Enter ';' to look for the next solution." ]
            input <- liftIO getLine'
            case parse space (string ";") input of
              Right _ -> handleSolutions sols
              Left _  -> cliInput .= Just input
      case solve prog q of
        []   -> liftIO $ putStrLn "No solution."
        sols -> handleSolutions sols

-- | Handle printing out the help message.
handleHelp :: MonadIO m => StateT CLIState m ()
handleHelp = liftIO $ putStrLn "TODO: Help message."
{-# INLINE handleHelp #-}

-- | Handle quitting the program.
handleQuit :: MonadIO m => StateT CLIState m ()
handleQuit = throw UserInterrupt
{-# INLINE handleQuit #-}
