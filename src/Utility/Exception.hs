{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Internal exception handling utilities.
--
-- They seem to be quite useful; I might consider exposing them in the future.
module Utility.Exception where

import           Control.Monad
import           Control.Exception
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Proxy

-- | A type class that can be converted from an Exception. We could then use
-- this representation of the same type to handle it.
class FromError e where
  -- | Convert an exception to the given type.
  fromError :: SomeException -> e

  -- | Check if the given exception is fatal.
  --
  -- By default, all exceptions are fatal.
  isFatal :: Proxy e -> SomeException -> Bool
  isFatal = const (const True)

  -- | A way to handle an error back to @IO@. Used by @handleErr'@ as the
  -- default way to handle the non-fatal errors.
  --
  -- Although it is used to handle non-fatal errors, we can still use this
  -- function to handle fatal errors as the outermost "wrapper".
  --
  -- By default, it ignores the error completely and does nothing.
  errHandler :: e -> IO ()
  errHandler _ = pure ()

-- | The most basic representation of an error, namely a simple @String@.
--
-- It treats @IOError@s as non-fatal and anything else as fatal. The handle
-- action is simply to print out error message.
newtype StringErr = StringErr String
  deriving (Eq, Ord)

instance Show StringErr where
  show :: StringErr -> String
  show (StringErr s) = s
  {-# INLINE show #-}

instance FromError StringErr where
  fromError :: SomeException -> StringErr
  fromError e = if isFatal (Proxy :: Proxy StringErr) e
    then StringErr $ "Fatal error:\n" ++ show e
    else StringErr $ show e
  {-# INLINE fromError #-}

  isFatal :: Proxy StringErr -> SomeException -> Bool
  isFatal _ e = case (fromException e :: Maybe IOError) of
    Just _  -> False
    Nothing -> True
  {-# INLINE isFatal #-}

  errHandler :: StringErr -> IO ()
  errHandler (StringErr s) = putStrLn s
  {-# INLINE errHandler #-}

-- | Non-fatal errors are handled using the given function. Fatal errors are
-- rethrown as a pure @ExceptT@.
handleErr :: forall e a. FromError e
          => (e -> IO a) -> ExceptT e IO a -> ExceptT e IO a
handleErr f = mapExceptT (`catch` handler)
  where
    handler (e :: SomeException)
      | isFatal (Proxy :: Proxy e) e = pure (Left (fromError e))
      | otherwise                    = Right <$> f (fromError e :: e)
{-# INLINE handleErr #-}

-- | Non-fatal errors are handled with the @errHandler@ function with a
-- provided value @a@. Fatal errors are rethrown as a pure @ExceptT@.
handleErr' :: forall e a. FromError e
           => IO a -> ExceptT e IO a -> ExceptT e IO a
handleErr' a = handleErr (errHandler >=> const a)
{-# INLINE handleErr' #-}

-- | Similar to @handleErr'@, but discards the result.
handleErr'_ :: forall e. FromError e => ExceptT e IO () -> ExceptT e IO ()
handleErr'_ = handleErr' (pure ())
{-# INLINE handleErr'_ #-}

-- | Similar to @handleErr@, but works in the @StateT@ monad.
--
-- If an error is caught, the state will not be updated.
handleErrS :: FromError e
           => (e -> IO a) -> StateT s (ExceptT e IO) a
           -> StateT s (ExceptT e IO) a
handleErrS f stateIO = StateT $ \s -> ExceptT $ do
  result <- runExceptT . handleErr (fmap (, s) <$> f) $ runStateT stateIO s
  return $ case result of
    Left err -> Left err
    Right as -> pure as
{-# INLINE handleErrS #-}

-- | Similar to @handleErr'@, but works in the @StateT@ monad.
--
-- If an error is caught, the state will not be updated.
handleErrS' :: FromError e
            => IO a -> StateT s (ExceptT e IO) a -> StateT s (ExceptT e IO) a
handleErrS' a = handleErrS (errHandler >=> const a)
{-# INLINE handleErrS' #-}

-- | Similar to @handleErr'_@, but works in the @StateT@ monad.
--
-- If an error is caught, the state will not be updated.
handleErrS'_ :: FromError e
             => StateT s (ExceptT e IO) () -> StateT s (ExceptT e IO) ()
handleErrS'_ = handleErrS' (pure ())
{-# INLINE handleErrS'_ #-}
