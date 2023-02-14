module Main where

import Parser
import Program
import Solver
import Utility


query :: String
query = "gt(X, Y), gt(Y, Z)."

main :: IO ()
main = do
  src <- readFile "test/programs/simpleNumbers.hrolog"
  case parseProgram src of
    Left err  -> putStrLn err
    Right ast -> do
      pPrint ast
      let Right q = parsePQuery query
      putStrLn $ "Query is " ++ pShow q
      let subs = head $ solve ast q
      let x    = foldl (flip substituteTerm) (VariableTerm "X") subs
      let y    = foldl (flip substituteTerm) (VariableTerm "Y") subs
      let z    = foldl (flip substituteTerm) (VariableTerm "Z") subs
      putStrLn $ "The solution for 'X' is " ++ pShow x
      putStrLn $ "The solution for 'Y' is " ++ pShow y
      putStrLn $ "The solution for 'Z' is " ++ pShow z
