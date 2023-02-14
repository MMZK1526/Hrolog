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
                let (subs, _) = head $ solve prog query
                lift $ subs `seq` putStrLn ("\nSolution:\n" ++ pShow subs)
      queryFeedback

main :: IO ()
main = void $ runStateT runProlog "test/programs/kangxi.hrolog"
