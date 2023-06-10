{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Utility.ParseCompleter where

import           Control.Applicative (Alternative(..))
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Foldable (asum)

newtype PC a
  = PC { unPC :: StateT String (MaybeT (WriterT [(String, String)] Maybe)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadWriter [(String, String)]
           , MonadState String )

written :: [(String, String)] -> PC a
written w = tell w >> PC (lift empty)
{-# INLINE written #-}

runPC :: PC a -> String -> Maybe (Maybe (a, String), [(String, String)])
runPC (PC pc) str = runWriterT (runMaybeT (runStateT pc str))
{-# INLINE runPC #-}

execPC :: PC a -> String -> [(String, String)]
execPC = (maybe [] snd .) . runPC
{-# INLINE execPC #-}

instance Alternative PC where
  empty :: PC a
  empty = PC . lift . lift $ lift empty
  {-# INLINE empty #-}

  (<|>) :: PC a -> PC a -> PC a
  pcX <|> pcY = do
    str <- get
    let resX = runPC pcX str
    case resX of
      Nothing      -> pcY
      Just (st, w) -> do
        case st of
          Nothing        -> written w
          Just (a, str') -> a <$ put str'

instance MonadPlus PC

tryBoth :: PC a -> PC a -> PC a
tryBoth pcX pcY = PC $ do
  str <- get
  let res1 = runPC pcX str
  let res2 = runPC pcY str
  case res1 of
    Just (_, w1) -> tell w1
    Nothing      -> pure ()
  case res2 of
    Just (_, w2) -> tell w2
    Nothing      -> pure ()
  let st = (res1 >>= fst) <|> (res2 >>= fst)
  (a, str') <- lift . MaybeT $ pure st
  put str'
  pure a

expect :: String -> PC String
expect matcher = do
  str <- get
  go str matcher
  where
    go [] ms = written [(ms, matcher)]
    go xs [] = matcher <$ put xs
    go (x : xs) (m : ms)
      | x == m    = go xs ms
      | otherwise = empty

satisfy :: (Char -> Bool) -> PC Char
satisfy p = do
  str <- get
  case str of
    []     -> empty
    x : xs -> if p x then x <$ put xs else empty

eatSpace :: PC ()
eatSpace = void . many $ satisfy (`elem` " \t\n")

notFollowedBy :: (Char -> Bool) -> PC ()
notFollowedBy p = do
  str <- get
  case str of
    []    -> pure ()
    x : _ -> when (p x) empty

choice :: [PC a] -> PC a
choice = asum
{-# INLINE choice #-}

choiceAll :: [PC a] -> PC a
choiceAll = foldl tryBoth empty
{-# INLINE choiceAll #-}
