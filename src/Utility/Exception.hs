{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Internal exception handling utilities.
--
-- They seem to be quite useful; I might consider exposing them in the future.
module Utility.Exception where

import           Data.Bifunctor
import           Data.Kind
import           Control.Monad
import           Control.Exception
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State

-- | A type class that can be converted from an Exception. We could then use
-- this representation of the same type to handle it.
class FromError e where
  -- | Convert an exception to the given type.
  fromError :: SomeException -> e

  -- | Check if the given exception is fatal.
  --
  -- By default, all exceptions are fatal.
  isFatal :: e -> Bool
  isFatal = const True

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
-- It takes a type level List to indicate which error types are not fatal.
--
-- In the @StringErr@ constructor, the first @Bool@ is used to indicate whether
-- the error is fatal or not. The second @String@ is used to store the actual
-- error message as a @String@.
data StringErr (l :: [Type]) = StringErr Bool String
  deriving (Eq, Ord)

-- | Can be used as the @l@ in @StringErr@ to indicate that all errors are
-- fatal.
type AllFatal = '[]

-- | Can be used as the @l@ in @StringErr@ to indicate that all errors except
-- @IOException@ are fatal.
type CatchIO = '[IOException]

instance Show (StringErr l) where
  show :: StringErr l -> String
  show (StringErr True s) = "Fatal Error:\n" ++ s
  show (StringErr _ s)    = s
  {-# INLINE show #-}

instance FromError (StringErr '[]) where
  fromError :: SomeException -> StringErr '[]
  fromError e = StringErr True $ show e
  {-# INLINE fromError #-}

  isFatal :: StringErr '[] -> Bool
  isFatal = const True

  errHandler :: StringErr '[] -> IO ()
  errHandler = print
  {-# INLINE errHandler #-}

instance (FromError (StringErr '[ts]), Exception t)
  => FromError (StringErr (t ': '[ts])) where
    fromError :: SomeException -> StringErr (t ': '[ts])
    fromError e = case fromException e :: Maybe t of
      Just t  -> StringErr False $ show t
      Nothing -> let StringErr b s = fromError e :: StringErr '[ts]
                 in  StringErr b s
    {-# INLINE fromError #-}

    isFatal :: StringErr (t ': '[ts]) -> Bool
    isFatal (StringErr b _) = b

    errHandler :: StringErr (t ': '[ts]) -> IO ()
    errHandler = print
    {-# INLINE errHandler #-}

-- | A type class representing a monad that can handle errors.
class MonadErrHandling m where
  -- | The pure error type that this monad can handle.
  --
  -- This type usually implements @FromError@ so that any impure error can be
  -- transformed into this type and then handled.
  type Err m

  -- | Handle all impure errors, either rethrowing them as a pure @FromError@
  -- catch them properly.
  --
  -- It takes a function that takes an error and returns an @Either@ of a
  -- transformed error of the same type (corresponding to uncaught) or a new
  -- value (corresponding to caught).
  dealWithErr :: (Err m -> IO (Either (Err m) a)) -> m a -> m a

  -- | Rethrow all impure errors into a pure @FromError@.
  wrapErr :: m a -> m a
  wrapErr = dealWithErr (pure . Left)
  {-# INLINE wrapErr #-}

-- | Basic @MonadErrHandling@ instance for @ExceptT@.
instance FromError e => MonadErrHandling (ExceptT e IO) where
  type Err (ExceptT e IO) = e

  dealWithErr :: (e -> IO (Either e a)) -> ExceptT e IO a -> ExceptT e IO a
  dealWithErr f
    = mapExceptT (`catch` (\(e :: SomeException) -> f (fromError e)))
  {-# INLINE dealWithErr #-}

-- | Handle errors in a @StateT@ monad. When an error is caught, the state is
-- restored to immediately before the action that caused the error.
instance MonadErrHandling m => MonadErrHandling (StateT s m) where
  type Err (StateT s m) = Err m

  dealWithErr :: (Err m -> IO (Either (Err m) a)) -> StateT e m a
              -> StateT e m a
  dealWithErr f stateIO = StateT
    $ \s -> dealWithErr (fmap (second (, s)) . f) (runStateT stateIO s)
  {-# INLINE dealWithErr #-}

-- | Non-fatal errors are handled using the given function. Fatal errors are
-- rethrown as a pure @ExceptT@.
handleErr :: FromError e => MonadErrHandling m => Err m ~ e
          => (e -> IO a) -> m a -> m a
handleErr f = dealWithErr $ \e -> if isFatal e
  then pure (Left e)
  else Right <$> f e
{-# INLINE handleErr #-}

-- | Non-fatal errors are handled with the @errHandler@ function with a
-- provided value @a@. Fatal errors are rethrown as a pure @ExceptT@.
handleErr' :: FromError e => MonadErrHandling m => Err m ~ e
           => IO a -> m a -> m a
handleErr' a = handleErr (errHandler >=> const a)
{-# INLINE handleErr' #-}

-- | Similar to @handleErr'@, but discards the result.
handleErr'_ :: FromError e => MonadErrHandling m => Err m ~ e => m () -> m ()
handleErr'_ = handleErr' (pure ())
{-# INLINE handleErr'_ #-}
