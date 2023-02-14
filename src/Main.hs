module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Map as M
import           Parser
import           Program
import           Solver.Prolog
import           Utility

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
    Right program -> do
      lift $ putStrLn (concat ["Program ", show path, " loaded."])
      let queryFeedback = forever $ do
            input <- lift getLine
            case parsePQuery input of
              Left qErr   -> do
                lift $ putStrLn "Error parsing the query!"
                lift $ putStrLn qErr
              Right query -> do
                let (subs, _) = head $ solve program query
                let x    = subs M.! "X"
                let y    = subs M.! "Y"
                let z    = subs M.! "Z"
                lift $ putStrLn $ "The solution for 'X' is " ++ pShow x
                lift $ putStrLn $ "The solution for 'Y' is " ++ pShow y
                lift $ putStrLn $ "The solution for 'Z' is " ++ pShow z
      queryFeedback

main :: IO ()
main = void $ runStateT runProlog "test/programs/simpleNumbers.hrolog"
