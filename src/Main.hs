module Main where

import Parser
import Program

import Control.Monad.Trans.State

main :: IO ()
main = case runState (parseT atom "lt(1, 2)") emptyProgram of
  (Left err, _)  -> putStrLn err
  (Right ast, p) -> print ast >> print p
