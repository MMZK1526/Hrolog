{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal exception handling utilities.
--
-- They seem to be quite useful; I might consider exposing them in the future.
module Utility.Exception where

import           Control.Monad
import           Control.Exception
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Proxy

-- | A type class that can be converted from an Exception.
class FromError e where
  -- | Convert an exception to the given type.
  fromError :: SomeException -> e

  -- | Check if the given exception is fatal.
  --
  -- By default, all exceptions are fatal.
  isFatal :: Proxy e -> SomeException -> Bool
  isFatal = const (const True)

  -- | A way to handle a non-fatal error. Used by @handleErr'@ as the default
  -- way.
  --
  -- By default, it ignores the error completely and does nothing.
  nonFatalHandler :: e -> IO ()
  nonFatalHandler _ = pure ()

-- | The most basic representation of an error, namely a simple @String@.
--
-- It treats @IOError@s as non-fatal and anything else as fatal. The handle
-- action is simply to print out error message.
newtype StringErr = StringErr String
  deriving Show

instance FromError StringErr where
  fromError :: SomeException -> StringErr
  fromError e = if isFatal (Proxy :: Proxy StringErr) e
    then StringErr $ "Fatal error:\n" ++ show e
    else StringErr $ show e

  isFatal :: Proxy StringErr -> SomeException -> Bool
  isFatal _ e = case (fromException e :: Maybe IOError) of
    Just _  -> False
    Nothing -> True

  nonFatalHandler :: StringErr -> IO ()
  nonFatalHandler (StringErr s) = putStrLn s

-- | Non-fatal errors are handled using the given function. Fatal errors are
-- rethrown as a pure @ExceptT@.
handleErr :: forall e a. FromError e
          => (e -> IO a) -> ExceptT e IO a -> ExceptT e IO a
handleErr f = mapExceptT (`catch` handler)
  where
    handler (e :: SomeException)
      | isFatal (Proxy :: Proxy e) e = pure (Left (fromError e))
      | otherwise                    = Right <$> f (fromError e :: e)

-- | Non-fatal errors are handled with the @nonFatalHandler@ function with a
-- provided value @a@. Fatal errors are rethrown as a pure @ExceptT@.
handleErr' :: forall e a. FromError e
           => IO a -> ExceptT e IO a -> ExceptT e IO a
handleErr' a = handleErr (nonFatalHandler >=> const a)
{-# INLINE handleErr' #-}

-- | Similar to @handleErr'@, but discards the result.
handleErr'_ :: forall e. FromError e => ExceptT e IO () -> ExceptT e IO ()
handleErr'_ = handleErr' (pure ())
{-# INLINE handleErr'_ #-}

-- | Handle any @IOException@ by simply recording them as @String@s, and rethrow
-- other exceptions as a pure @ExceptT@, in a @StateT@ context.
--
-- If an error is caught, the state will not be updated.
handleErrS :: FromError e
           => StateT s (ExceptT e IO) () -> StateT s (ExceptT e IO) ()
handleErrS stateIO = StateT $ \s -> ExceptT $ do
  result <- runExceptT . handleErr' (pure ((), s)) $ runStateT stateIO s
  return $ case result of
    Left err -> Left err
    Right as -> pure as
