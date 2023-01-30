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
  Right ast -> pPrint ast
      >> print (solve ast (Atom (Predicate {_predicateName = "newborn", _predicateArity = 1}) [VariableTerm "X"]))
