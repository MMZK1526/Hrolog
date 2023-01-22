module Main where

import Parser
import Program

import Control.Monad.Trans.State

main :: IO ()
main = case runState (parseT termParser "_") emptyProgram of
  (Left err, _)  -> putStrLn err
  (Right ast, p) -> print ast >> print p
