module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Parser
import           Program
import           Solver.Prolog

testQuery :: String
testQuery = "gt(X, Y), gt(Y, Z)."

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
                let solutions = solve prog query
                -- solutions <- lift $ solveIO prog query
                case solutions of
                  []              -> lift $ putStrLn "No fresh solution.\n"
                  ((subs, _) : _) -> do
                    let solStr = pShow subs
                    solStr `seq` lift $ putStrLn ("\nSolution:\n" ++ solStr)
      queryFeedback

main :: IO ()
main = void $ runStateT runProlog "test/programs/simpleNumbers.hrolog"
