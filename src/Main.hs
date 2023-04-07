{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Text.Megaparsec as P

import           Parser
import           Program
import           Solver.Prolog
import           Utility.Exception

testQuery :: String
testQuery = "gt(X, Y), gt(Y, Z)."

-- | The input types of the CLI.
data InputType = InputTypeFilePath FilePath
               | InputTypePQuery PQuery
               | InputTypeReload

-- | The state of the CLI.
data CLIState = CLIState { _cliSfilePath :: Maybe FilePath
                         , _cliProgram   :: Maybe Program
                         , _cliPQuery    :: Maybe PQuery }

makeLenses ''CLIState

-- | The initial @CLIState@.
emptyCLIState :: CLIState
emptyCLIState = CLIState Nothing Nothing Nothing

-- | The CLI loop. It takes an input, parses it, and executes the corresponding,
-- forever.
feedbackloop :: StateT CLIState (ExceptT String IO) ()
-- The "forever" indicates that the loop will never terminate unless there is
-- an uncaught exception.
-- "handleStateErr" is a utility function that catches all "IOError"s by
-- printing them out. In other words, if an "IOError" is thrown, the program
-- will ignore the current progress, print out the error, and continue to the
-- next loop. 
-- On the other hand, it does not catch other errors (such as user-induced
-- termination). In this case, the function transforms this error into a pure
-- "String" exception wrapped in an "ExceptT", and the program will break from
-- "forever" and terminate with a message corresponding to the content of the
-- "String".
feedbackloop = forever . handleStateErr $ do
  mProg <- use cliProgram -- Get the program from the state.
  input <- liftIO getLine -- Get the input from the user.
  -- Parse the input.
  case evalState (parseT inputP input) mProg of
    -- If the input is invalid, print an error message.
    Left err                            -> liftIO $ do
      putStrLn "Error parsing the input!"
      putStrLn err
    -- A @Nothing@ means that the input is a query, but the program is not
    -- loaded. In this case, we print an error message.
    Right Nothing                       ->
      liftIO $ putStrLn "Cannot take query without a program loaded."
    -- If the input is a file path, we load the program, parse it, and store it
    -- in the state.
    Right (Just (InputTypeFilePath fp)) -> handleNewProgram fp
    Right (Just InputTypeReload)        -> handleReload
    Right (Just (InputTypePQuery q))    -> liftIO $ putStrLn "TODO"

runProlog :: StateT FilePath IO ()
runProlog = do
  path <- get
  src  <- lift $ readFile path
  case parseProgram src of
    Left pErr     -> do
      lift $ putStrLn "Error parsing the program!"
      lift $ putStrLn pErr
    Right prog -> do
      lift $ putStrLn (concat ["Program ", show path, " loaded.\n"])
      let queryFeedback = forever $ do
            input <- lift getLine
            case parsePQuery input of
              Left qErr   -> do
                lift $ putStrLn "Error parsing the query!"
                lift $ putStrLn qErr
              Right query -> do
                -- let solutions = solve prog query
                solutions <- lift $ solveIO prog query
                case solutions of
                  []         -> lift $ putStrLn "No fresh solution.\n"
                  (subs : _) -> do
                    let solStr = prettifySolution subs
                    solStr `seq` lift $ putStrLn ("\nSolution:\n" ++ solStr)
      queryFeedback

main :: IO ()
main = do
  result <- runExceptT . void $ do
    liftIO $ putStrLn "Welcome to Hrolog!"
    runStateT feedbackloop emptyCLIState
  case result of
    Left err -> putStrLn err
    Right _  -> pure ()


--------------------------------------------------------------------------------
-- Input Parsers
--------------------------------------------------------------------------------

-- | Parse an input. The @Program@ in the inner @StateT@ is provided by the
-- "Program" in the @CLIState@.
inputP :: Monad m => ParserT (StateT (Maybe Program) m) (Maybe InputType)
inputP = P.choice [ Just . InputTypeFilePath <$> inputFilePath
                  , Just InputTypeReload <$ inputReload
                  , fmap InputTypePQuery <$> inputPQuery ]

inputFilePath :: Monad m => ParserT m FilePath
inputFilePath = string ":l" >> P.many P.anySingle

inputPQuery :: Functor f => Monad m => ParserT (StateT (f Program) m) (f PQuery)
inputPQuery = P.optional (string "<-") >> pQuery

inputReload :: Monad m => ParserT m ()
inputReload = void $ string ":r"


--------------------------------------------------------------------------------
-- Input Handlers & Helpers
--------------------------------------------------------------------------------

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

handleReload :: MonadIO m => StateT CLIState m ()
handleReload = do
  mfp <- use cliSfilePath -- Get the file path from the state.
  case mfp of
    -- If the file path is not stored, let the user know.
    Nothing -> liftIO $ putStrLn "No program loaded."
    -- If the file path is stored, reload the program from the file.
    Just fp -> handleNewProgram fp
