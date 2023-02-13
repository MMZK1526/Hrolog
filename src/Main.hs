module Main where

import Parser
import Program
import Solver
import Utility

foo :: String
foo = "proud(X) <- parent(X, Y), newborn(Y).\n\
      \parent(X, Y) <- father(X, Y).\n\
      \parent(X, Y) <- mother(X, Y).\n\
      \father(adam, mary).\n\
      \newborn(mary).\n"

fatherQuery :: Atom
fatherQuery = Atom (Predicate {_predicateName = "father", _predicateArity = 2})
                   [VariableTerm "X", VariableTerm "Y"]

proudQuery :: Atom
proudQuery = Atom (Predicate {_predicateName = "proud", _predicateArity = 1})
                   [VariableTerm "Z"]

main :: IO ()
main = case parseProgram " <- 9()." of
  Left err  -> putStrLn err
  Right ast -> do
    pPrint ast
    let subs = head $ solve ast [proudQuery]
    let result = foldl (flip substituteTerm) (VariableTerm "Z") subs
    putStrLn $ "The solution for 'Z' is " ++ pShow result
