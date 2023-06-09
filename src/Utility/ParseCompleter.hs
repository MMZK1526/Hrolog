{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Utility.ParseCompleter where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe

newtype PC a = PC (WriterT [(String, String)] (StateT String Maybe) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadWriter [(String, String)]
           , MonadState String )

runPC :: PC a -> String -> Maybe (a, [(String, String)])
runPC (PC pc) = evalStateT (runWriterT pc)
{-# INLINE runPC #-}

execPC :: PC a -> String -> [(String, String)]
execPC (PC pc) = fromMaybe [] . evalStateT (execWriterT pc)
{-# INLINE execPC #-}

instance Alternative PC where
  empty :: PC a
  empty = PC $ lift empty
  {-# INLINE empty #-}

  (<|>) :: PC a -> PC a -> PC a
  pcX <|> pcY = PC $ do
    str <- get
    let res1 = runPC pcX str
    let res2 = runPC pcY str
    case res1 of
      Just (_, o1) -> tell o1
      Nothing      -> pure ()
    case res2 of
      Just (_, o2) -> tell o2
      Nothing      -> pure ()
    lift . lift $ fmap fst (res1 <|> res2)

instance MonadPlus PC where
  mzero :: PC a
  mzero = empty
  {-# INLINE mzero #-}

  mplus :: PC a -> PC a -> PC a
  mplus = (<|>)
  {-# INLINE mplus #-}

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

choice :: [PC a] -> PC a
choice = asum
{-# INLINE choice #-}