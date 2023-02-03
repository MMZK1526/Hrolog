module Main where

import Parser
import Program
import Solver

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

newbornQuery :: Atom
newbornQuery = Atom (Predicate {_predicateName = "newborn", _predicateArity = 1})
                   [ConstantTerm $ Constant "mary"]

main :: IO ()
main = case parseProgram foo of
  Left err  -> putStrLn err
  Right ast -> do
    pPrint ast
    print (solve ast [proudQuery])
