{-# LANGUAGE ScopedTypeVariables #-}

module Utility.Exception where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State

-- | Handle any @IOError@ by simply recording them as @String@s, and rethrow
-- other exceptions as a pure @ExceptT@, in a @StateT@ context.
handleStateErr :: StateT s (ExceptT String IO) () -> StateT s (ExceptT String IO) ()
handleStateErr stateIO = StateT $ \s -> ExceptT $ do
  result <- runExceptT . handleErr . void $ runStateT stateIO s
  return $ case result of
    Left err -> Left err
    Right _  -> pure ((), s)

-- | Handle any @IOError@ by simply recording them as @String@s, and rethrow
-- other exceptions as a pure @ExceptT@.
handleErr :: ExceptT String IO () -> ExceptT String IO ()
handleErr = mapExceptT (`catches` [Handler logIOErrors, Handler rethrowOtherErrors])
  where
    logIOErrors (e :: IOError)              = pure <$> print e
    rethrowOtherErrors (e :: SomeException) = pure (Left ("Fatal Exception: " ++ show e))
