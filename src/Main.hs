module Main where

import Parser
import Program

main :: IO ()
main = case parseProgram "even(0).\n <- odd(1).\n <- even(X), odd(X)." of
  Left err  -> putStrLn err
  Right ast -> print ast
