module Main where

import Parser
import Program
import Solver (solve)

foo :: String
foo = "proud(X) <- parent(X, Y), newborn(Y).\n\
      \parent(X, Y) <- father(X, Y).\n\
      \parent(X, Y) <- mother(X, Y).\n\
      \father(adam, mary).\n\
      \newborn(mary).\n"

main :: IO ()
main = case parseProgram foo of
  Left err  -> putStrLn err
  Right ast -> do
    pPrint ast
    print (solve ast [ Atom (Predicate {_predicateName = "father", _predicateArity = 2}) [VariableTerm "Y", VariableTerm "X"] ])
