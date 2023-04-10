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

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Coerce
import           Data.Kind

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
  {-# INLINE isFatal #-}

instance (FromError (StringErr '[ts]), Exception t)
  => FromError (StringErr (t ': '[ts])) where
    fromError :: SomeException -> StringErr (t ': '[ts])
    fromError e = case fromException e :: Maybe t of
      Just t  -> StringErr False $ show t
      Nothing -> coerce (fromError e :: StringErr '[ts])
    {-# INLINE fromError #-}

    isFatal :: StringErr (t ': '[ts]) -> Bool
    isFatal (StringErr b _) = b
    {-# INLINE isFatal #-}

-- | A type class representing a monad that can handle errors.
class MonadIO m => MonadErrHandling m where
  -- | The monolithic error type that this monad can handle.
  --
  -- This type usually implements @FromError@ so that any impure error can be
  -- transformed into this type and then be handled.
  type Err m

  -- | Handle all impure errors, either rethrowing them as a pure @FromError@
  -- catch them properly.
  --
  -- It takes a function that takes an error and returns an @Either@ of a
  -- transformed error of the same type (corresponding to uncaught) or a new
  -- value (corresponding to caught).
  dealWithErr :: (Err m -> m (Either (Err m) a)) -> m a -> m a

  -- | Rethrow all impure errors into a pure @FromError@.
  wrapErr :: m a -> m a
  wrapErr = dealWithErr (pure . Left)
  {-# INLINE wrapErr #-}

-- | Basic @MonadErrHandling@ instance for @ExceptT@.
instance FromError e => MonadErrHandling (ExceptT e IO) where
  type Err (ExceptT e IO) = e

  dealWithErr :: (e -> ExceptT e IO (Either e a)) -> ExceptT e IO a -> ExceptT e IO a
  dealWithErr f = mapExceptT (`catch` (fmap join . runExceptT . f . fromError))
  {-# INLINE dealWithErr #-}

-- | Handle errors in a @StateT@ monad. When an error is caught, the state is
-- restored to immediately before the action that caused the error. However,
-- the handler may change the state as well, although it is recommended to not
-- do so.
instance MonadErrHandling m => MonadErrHandling (StateT s m) where
  type Err (StateT s m) = Err m

  dealWithErr :: (Err m -> StateT s m (Either (Err m) a)) -> StateT s m a
              -> StateT s m a
  dealWithErr f stateIO = StateT
    $ \s -> dealWithErr (worker s) (runStateT stateIO s)
    where
      worker s e = do
        (result, s') <- runStateT (f e) s
        return $ second (, s') result
  {-# INLINE dealWithErr #-}

-- | Non-fatal errors are handled using the given function. Fatal errors are
-- rethrown as a pure @ExceptT@.
handleErr :: FromError (Err m) => MonadErrHandling m
          => (Err m -> m a) -> m a -> m a
handleErr f = dealWithErr $ \e -> if isFatal e
  then pure (Left e)
  else Right <$> f e
{-# INLINE handleErr #-}
