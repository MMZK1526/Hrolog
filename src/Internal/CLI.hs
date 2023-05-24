{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | This modules contains the main function of the program. The main function
-- in @Main.hs@ simply reexports the @runCLI@ function here.
module Internal.CLI where

import           Control.Exception (AsyncException(UserInterrupt))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Console.Haskeline
import           System.Directory
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import           Internal.CLI.Type
import           Parser
import           Program
import           Solver.Hrolog
import           Utility.Exception
import           Utility.Parser
import           Utility.PP

-- | The main function of the program. It runs the CLI feedback loop, handling
-- any errors that may propagate to this stage.
runCLI :: IO ()
-- Since all benign errors are handled by the feedback loop already, we only
-- need to handle fatal errors here.
-- For serious and fatal errors, they will be wrapped in an "ExceptT". We read
-- it from "result" and handle it using "errHandler".
runCLI = do
  result <- runExceptT . void $ do
    liftIO $ putStrLn "Welcome to Hrolog!"
    evalStateT (runInputT settings $ feedbackloop (const $ pure ()))
               initCLIState
  case result of
    Left err -> errHandler err Nothing
    Right _  -> pure ()
  where
    cmds                = [":load", ":reload", ":help", ":quit", "<-"]
    settings            = (defaultSettings @IO) { complete = completer }
    trimStart           = dropWhile isSpace
    prefixSplit "" ys   = Just ys
    prefixSplit _ ""    = Nothing
    prefixSplit (x : xs) (y : ys)
      | x == y    = prefixSplit xs ys
      | otherwise = Nothing
    completer           = completeCmd `fallbackCompletion` completeFilename
    completeCmd (l, []) = do
      let completers = (`concatMap` cmds)
                     $ \w -> case prefixSplit (trimStart $ reverse l) w of
            Nothing -> []
            Just ys -> [Completion (ys ++ " ") w False]
      pure (l, completers)
    completeCmd x       = noCompletion x

-- | The CLI feedback loop. It takes an input, parses it, and executes the
-- corresponding instruction, forever.
--
-- It takes a callback that is called at start of each loop. This callback is
-- useful for testing purposes as it has access to the state of the program.
feedbackloop :: (CLIState -> IO ())
             -> InputT (StateT CLIState (ExceptT (TaggedError CLIError) IO)) ()
-- The "forever" indicates that the loop will never terminate unless there is
-- an uncaught exception.
-- "handleErrS" is a utility function that catches all benign errors by
-- printing them out. In other words, if an "IOError" is thrown, the program
-- will ignore the current progress, print out the error, and continue to the
-- next loop.
-- On the other hand, it does not catch serious and fatal errors (such as
-- user-induced termination). In this case, the function transforms this error
-- into a pure "TaggedError CLIError" wrapped in an "ExceptT", and the program
-- will break from "forever" and terminate with a message corresponding to the
-- content of the "Text". The pure error can then be handled by another
-- handler.
feedbackloop callback = forever $ do
  lift get >>= liftIO . callback -- Call the callback
  lift $ cliIteration += 1 -- Increment the iteration counter
  handleErr errHandlerS $ do
    mProg  <- lift $ use cliProgram -- Get the program from the state
    mInput <- do -- Get the user input
      mInput  <- lift $ use cliInput
      case mInput of
        -- If the input is already stored in the state, we use it.
        Just input -> Just input <$ lift (cliInput .= Nothing)
        -- Otherwise, we prompt the user for input.
        Nothing    -> getLine'
    -- Parse the input.
    case mInput of
      Nothing    -> handleQuit
      Just input -> case evalState (parseT space inputP input) mProg of
        -- If the input is invalid, print an error message.
        Left err                            -> liftIO $ do
          T.putStrLn "Error parsing the input:"
          T.putStrLn err
        -- A @Nothing@ means that the input is a query, but the program is not
        -- loaded. In this case, we print an error message.
        Right Nothing                       ->
          liftIO $ putStrLn "Cannot take query without a program loaded."
        -- Dispatch the input to the corresponding handler.
        Right (Just inputType)              -> case inputType of
          InputTypeFilePath fp -> handleLoad fp
          InputTypeReload      -> handleReload
          InputTypePQuery q    -> handlePQuery q
          InputTypeHelp        -> handleHelp
          InputTypeQuit        -> handleQuit
    lift $ cliErr .= Nothing -- Reset the error state


--------------------------------------------------------------------------------
-- Input Parsers & Helpers
--------------------------------------------------------------------------------

-- | Parse an input. The @Program@ in the inner @StateT@ is provided by the
-- "Program" in the @CLIState@.
inputP :: Monad m => ParserT (StateT (Maybe Program) m) (Maybe InputType)
inputP = P.choice [ Just . InputTypeFilePath <$> inputFilePath
                  , Just InputTypeReload <$ inputReload
                  , Just InputTypeQuit <$ inputQuit
                  , Just InputTypeHelp <$ inputHelp
                  , fmap InputTypePQuery <$> pQuery' True ]

-- | Parse a file path.
inputFilePath :: Monad m => ParserT m FilePath
inputFilePath = P.try (string ":l") P.<|> string ":load" >> P.many P.anySingle
{-# INLINE inputFilePath #-}

-- | Parse a reload command.
inputReload :: Monad m => ParserT m ()
inputReload = void $ P.try (string ":r") P.<|> string ":reload"
{-# INLINE inputReload #-}

-- | Parse a help command.
inputHelp :: Monad m => ParserT m ()
inputHelp = void $ P.try (string ":h") P.<|> string ":help"
{-# INLINE inputHelp #-}

-- | Parse a quit command.
inputQuit :: Monad m => ParserT m ()
inputQuit = void $ P.try (string ":q") P.<|> string ":quit"
{-# INLINE inputQuit #-}

-- | Parse spaces.
space :: Monad m => ParserT m ()
space = L.space P.space1 P.empty P.empty

-- | Parse a string with space after it.
string :: Monad m => Text -> ParserT m Text
string str = L.lexeme space $ P.string str <* P.notFollowedBy P.alphaNumChar

-- | Repeatedly read input from the user until a non-empty @Text@ is read.
getLine' :: MonadIO m => MonadMask m => InputT m (Maybe Text)
getLine' = do
  mInput <- fmap T.pack <$> getInputLine "> "
  case mInput of
    Nothing    -> pure Nothing
    Just input -> case parse space (pure ()) input of
      Left _  -> pure $ Just input
      Right _ -> getLine'


--------------------------------------------------------------------------------
-- Input Handlers & Helpers
--------------------------------------------------------------------------------

-- | Error handler for the feedback loop.
--
-- It stores the error in the state, and then calls the error handler for the
-- main function (which is identical to what we want to handle here).
errHandlerS :: TaggedError CLIError
            -> InputT (StateT CLIState (ExceptT (TaggedError CLIError) IO)) ()
errHandlerS err = lift $ cliErr ?= err >> get >>= errHandler err . Just

-- | The error handler for the main function for both fatal and non-fatal
-- errors.
errHandler :: MonadErrHandling (TaggedError CLIError) m
           => TaggedError CLIError -> Maybe CLIState -> m ()
errHandler (TaggedError e Nothing) _      = liftIO $ do
  putStrLn "Fatal Error!"
  putStrLn "The developer would be most grateful if you can report this."
  putStrLn "Please send this error message to mmzk1526@outlook.com."
  throwM e
errHandler (TaggedError _ (Just err)) mCS = case err of
  DNEErr mfp      -> do
    curDir <- liftIO getCurrentDirectory
    let fileDNEErrMsg = case mfp of
          Just fp -> concat ["File ", show fp, " does not exist."]
          _       -> "File does not exist."
    let curDirMsg     = concat ["Current directory is ", show curDir, "."]
    liftIO $ putStrLn $ unlines [fileDNEErrMsg, curDirMsg]
  IOErr e -> liftIO $ putStrLn ("IO Error:\n" ++ e)
  UserInter       -> liftIO $ putStrLn "Quit by user."
  InternalErr msg -> liftIO $ do
    putStrLn "Internal Error:"
    putStrLn msg
    forM_ mCS pPrint
    putStrLn "The developer would be most grateful if you can report this."
    putStrLn "Please send this error message to mmzk1526@outlook.com.\n"
    putStrLn "Meanwhile, you could continue with your other queries."

-- | Handle loading a program from a path.
handleLoad :: MonadIO m => FilePath -> InputT (StateT CLIState m) ()
handleLoad fp = do
  lift $ cliFilePath ?= fp -- Store the file path in the state.
  newProg <- liftIO $ T.readFile fp -- Read the program from the file.
  case parseProgram newProg of
    -- If the program is invalid, print an error message.
    Left pErr  -> liftIO $ do
      T.putStrLn "Error parsing the program!"
      T.putStrLn pErr
    -- If the program is valid, store it in the state.
    Right prog -> do
      lift $ cliProgram ?= prog
      liftIO $ T.putStrLn (T.concat ["Program ", pShow fp, " loaded:"])
      liftIO $ T.putStrLn $ prettifyProgram prog

-- | Handle reloading the program from the file path stored in the state.
handleReload :: MonadIO m => InputT (StateT CLIState m) ()
handleReload = do
  mfp <- lift $ use cliFilePath -- Get the file path from the state.
  case mfp of
    -- If the file path is not stored, let the user know.
    Nothing -> liftIO $ putStrLn "No program loaded.\n"
    -- If the file path is stored, reload the program from the file.
    Just fp -> handleLoad fp

-- | Handle parsing and solving with a query.
handlePQuery :: MonadIO m => MonadMask m
             => PQuery -> InputT (StateT CLIState m) ()
handlePQuery q = do
  lift $ cliPQuery ?= q -- Store the query in the state.
  mProg <- lift $ use cliProgram -- Get the program from the state.
  case mProg of
    -- If the program is not stored, let the user know.
    Nothing -> liftIO $ putStrLn "No program loaded."
    -- If the program is stored, solve the query.
    Just prog -> do
      let handleSolutions []           = liftIO $ putStrLn "No more solutions."
          handleSolutions (sol : sols) = do
            liftIO . T.putStrLn $ T.concat
              [ "\nSolution:\n", prettifySolution sol, "\n"
              , "Enter ';' to look for the next solution." ]
            mInput <- getLine'
            case mInput of
              Nothing    -> handleQuit
              -- If the user enters a semicolon, print the next solution.
              -- Otherwise, store the input in the state to be executed at the
              -- next iteration of the feedback loop.
              Just input -> case parse space (string ";") input of
                Right _ -> handleSolutions sols
                Left _  -> lift $ cliInput ?= input
            
      case solve prog q of
        []   -> liftIO $ putStrLn "No solution."
        sols -> handleSolutions sols

-- | Handle printing out the help message.
handleHelp :: MonadIO m => InputT (StateT CLIState m) ()
handleHelp = liftIO $ putStrLn "TODO: Help message."
{-# INLINE handleHelp #-}

-- | Handle quitting the program.
handleQuit :: MonadIO m => MonadThrow m => InputT (StateT CLIState m) ()
handleQuit = throwM UserInterrupt
{-# INLINE handleQuit #-}
