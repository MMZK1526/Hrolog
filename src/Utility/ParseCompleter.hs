{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- | Helper for auto-completion.
module Utility.ParseCompleter where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer

-- | Auto-completion as monadic combinators.
--
-- See the combinators for details.
newtype PC a
  = PC { unPC :: StateT String (MaybeT (WriterT [(String, String)] Maybe)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadWriter [(String, String)]
           , MonadState String )

-- | Run the completion parser. It takes the combinator and a @String@ to parse.
--
-- There are three possible outcomes.
-- 1. The @String@ does not satisfy the grammar and the parsing fails. In this
--    case, the result is @Nothing@.
-- 2. The @String@ is a prefix of a valid grammar. In this case, the result is
--    @Just (Nothing, w)@, where @w@ is the list of possible completions.
-- 3. The @String@ is a valid grammar, but there are remaining characters. In
--    this case, the result is @Just (Just (a, str'), w)@, where @a@ is the
--    result of the parsing, @str'@ is the remaining @String@, and @w@ (usually
--    empty) is the list of possible completions.
--
-- >>> runPC (expect "abc") "ab"
-- Just (Nothing,[("c","abc")])
-- >>> runPC (expect "abc") "abcd"
-- Just (Just ("abc","d"),[])
-- >>> runPC (expect "abc") "abx"
-- Nothing
runPC :: PC a -> String -> Maybe (Maybe (a, String), [(String, String)])
runPC (PC pc) str = runWriterT (runMaybeT (runStateT pc str))
{-# INLINE runPC #-}

-- | Run the completion parser and return the list of possible completions.
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

-- | Try both parsers and return the result of the first one that succeeds. It
-- logs the possible completions of both parsers.
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

-- | Log the given completions and ignore everything that follows.
written :: [(String, String)] -> PC a
written w = tell w >> PC (lift empty)
{-# INLINE written #-}

-- | Expect a string. If the string is a prefix of the input, it succeeds and
-- returns the remaining input as completions. If the string fully consumes
-- the expected string, it succeeds and returns the remaining input without
-- logging completions. Otherwise, it fails.
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
{-# INLINE eatSpace #-}

notFollowedBy :: (Char -> Bool) -> PC ()
notFollowedBy p = do
  str <- get
  case str of
    []    -> pure ()
    x : _ -> when (p x) empty

-- | Use the first parser that succeeds.
choice :: [PC a] -> PC a
choice = msum
{-# INLINE choice #-}

-- | Try all the parsers and return the result of the first one that succeeds,
-- logging the possible completions of all parsers.
choiceAll :: [PC a] -> PC a
choiceAll = foldl tryBoth empty
{-# INLINE choiceAll #-}
