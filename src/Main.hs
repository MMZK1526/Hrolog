module Main where

import Parser
import Program

foo :: String
foo = "proud(X) <- parent(X, Y), newborn(Y).\n\
      \parent(X, Y) <- father(X, Y).\n\
      \parent(X, Y) <- mother(X, Y).\n\
      \father(adam, mary).\n\
      \newborn(mary).\n"

main :: IO ()
main = case parseProgram foo of
  Left err  -> putStrLn err
  Right ast -> print ast
