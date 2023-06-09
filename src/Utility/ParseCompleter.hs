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

runPC :: PC a -> String -> Maybe ((a, [(String, String)]), String)
runPC (PC pc) = runStateT (runWriterT pc)
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
      Just ((_, o1), _) -> tell o1
      Nothing      -> pure ()
    case res2 of
      Just ((_, o2), _) -> tell o2
      Nothing      -> pure ()
    put $ maybe str snd (res1 <|> res2)
    lift . lift $ fmap (fst . fst) (res1 <|> res2)

instance MonadPlus PC where
  mzero :: PC a
  mzero = empty
  {-# INLINE mzero #-}

  mplus :: PC a -> PC a -> PC a
  mplus = (<|>)
  {-# INLINE mplus #-}

expect :: String -> PC (Maybe String)
expect matcher = do
  str <- get
  go str matcher
  where
    go [] ms = Nothing <$ tell [(ms, matcher)]
    go xs [] = Just matcher <$ put xs
    go (x : xs) (m : ms)
      | x == m    = go xs ms
      | otherwise = mzero

satisfy :: (Char -> Bool) -> PC Char
satisfy p = do
  str <- get
  case str of
    []     -> mzero
    x : xs -> if p x then x <$ put xs else mzero

eatSpace :: PC ()
eatSpace = void . many $ satisfy (`elem` " \t\n")

notFollowedBy :: (Char -> Bool) -> PC ()
notFollowedBy p = do
  str <- get
  case str of
    []    -> pure ()
    x : _ -> when (p x) mzero

choice :: [PC a] -> PC a
choice = asum
{-# INLINE choice #-}
