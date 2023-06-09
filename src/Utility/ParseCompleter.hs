{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utility.ParseCompleter where

import           Control.Monad.State
import           Control.Monad.Writer

newtype PC a = PC (WriterT [(String, String)] (State String) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadWriter [(String, String)]
           , MonadState String )

runPC :: PC a -> String -> (a, [(String, String)])
runPC (PC pc) = evalState (runWriterT pc)
{-# INLINE runPC #-}

execPC :: PC a -> String -> [(String, String)]
execPC = (snd .) . runPC
{-# INLINE execPC #-}

expect :: String -> PC ()
expect matcher = do
  str <- get
  go str matcher
  where
    go [] ms = tell [(ms, matcher)]
    go xs [] = put xs
    go (x : xs) (m : ms)
      | x == m    = go xs ms
      | otherwise = pure ()

choice :: [PC ()] -> PC ()
choice []       = pure ()
choice (x : xs) = do
  str <- get
  tell (execPC x str)
  choice xs
