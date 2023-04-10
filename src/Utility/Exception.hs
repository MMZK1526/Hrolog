{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

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
--
-- There are three levels of error fatality:
-- 1. Fatal errors are those that are not recoverable. Those errors will result
-- in @Left@s from @fromError@. They will usually be handled at the outermost
-- level of the program using the good old @catch@ or @handle@.
-- 2. Serious errors are those that are potentially recoverable but marked as
-- @True@ by @isSerious@.
-- 3. Benign errors are those that are not serious and are marked as @False@ by
-- @isSerious@.
--
-- Note that this class does not care about the result from @isSerious@. In
-- other words, it does not care whether the error is serious or not. However,
-- the utility function @handleErr@ does care about this, and it will handle
-- benign errors but will rethrow serious errors as a pure @ExceptT@.
class FromError e where
  -- | Convert an exception to the given type.
  fromError :: SomeException -> Either SomeException e

  -- | Check if the given exception is serious.
  --
  -- By default, all exceptions are serious.
  isSerious :: e -> Bool
  isSerious = const True

-- | The most basic representation of an error, namely a simple @String@.
--
-- It takes two type level List to indicate which error types are benign and
-- which are serious. Everything else are considered fatal.
--
-- In the @StringErr@ constructor, the first @Bool@ is used to indicate whether
-- the error is fatal or not. The second @String@ is used to store the actual
-- error message as a @String@.
data StringErr (benign :: [Type]) (serious :: [Type]) = StringErr Bool String
  deriving (Eq, Ord)

-- | Can be used as the @l@ in @StringErr@ to indicate that are no benign
-- errors.
type NoBenign = '[]

-- | Can be used as the @l@ in @StringErr@ to indicate that are no serious
-- errors.
type NoSerious = '[]

-- | Can be used as the @l@ in @StringErr@ to indicate that @IOException@s are
-- benign.
type BenignIO = '[IOException]

-- | Can be used as the @l@ in @StringErr@ to indicate that @IOException@s are
-- serious.
type SeriousIO = '[IOException]

instance Show (StringErr benign serious) where
  show ::StringErr benign serious -> String
  show (StringErr True s) = "Fatal Error:\n" ++ s
  show (StringErr _ s)    = s
  {-# INLINE show #-}

-- | An instance where all exceptions are fatal. The @isSerious@ function is
-- useless.
instance FromError (StringErr '[] '[]) where
  fromError :: SomeException -> Either SomeException (StringErr '[] '[])
  fromError = Left
  {-# INLINE fromError #-}

  isSerious :: StringErr '[] '[] -> Bool
  isSerious = const True
  {-# INLINE isSerious #-}

-- | An instance where there are no benign errors. We recurse on the type of
-- the serious errors.
instance (FromError (StringErr '[] '[ss]), Exception s)
  => FromError (StringErr '[] '[s, ss]) where
    fromError :: SomeException -> Either SomeException (StringErr '[] '[s, ss])
    fromError e = case fromException e :: Maybe s of
      Just t  -> Right $ StringErr True $ show t
      Nothing ->
        coerce (fromError e :: Either SomeException (StringErr '[] '[ss]))
    {-# INLINE fromError #-}

    isSerious :: StringErr '[] '[s, ss] -> Bool
    isSerious (StringErr b _) = b
    {-# INLINE isSerious #-}

-- | We recurse on the type of the benign errors.
instance (FromError (StringErr '[bs] '[ss]), Exception b)
  => FromError (StringErr '[b, bs] '[ss]) where
    fromError :: SomeException -> Either SomeException (StringErr '[b, bs] '[ss])
    fromError e = case fromException e :: Maybe b of
      Just t  -> Right $ StringErr True $ show t
      Nothing ->
        coerce (fromError e :: Either SomeException (StringErr '[bs] '[ss]))
    {-# INLINE fromError #-}

    isSerious :: StringErr '[b, bs] '[ss] -> Bool
    isSerious (StringErr b _) = b
    {-# INLINE isSerious #-}

-- | A type class representing a monad that can handle errors.
class MonadIO m => MonadErrHandling m where
  -- | The monolithic error type that this monad can handle.
  --
  -- This type usually implements @FromError@ so that any impure error can be
  -- transformed into this type and then be handled.
  type Err m

  -- | Handle all impure errors, either rethrowing them or catch them properly.
  --
  -- It takes a function that takes an error and returns an @Either@ of a
  -- transformed error of the same type (corresponding to uncaught) or a new
  -- value (corresponding to caught).
  dealWithErr :: (Either SomeException (Err m) -> m (Either (Err m) a)) -> m a -> m a

  -- | Rethrow all non-fatal errors into a pure @FromError@.
  wrapErr :: m a -> m a
  wrapErr = dealWithErr $ \case
    Left err -> throw err
    Right e  -> pure $ Left e
  {-# INLINE wrapErr #-}

-- | Basic @MonadErrHandling@ instance for @ExceptT@.
instance FromError e => MonadErrHandling (ExceptT e IO) where
  type Err (ExceptT e IO) = e

  dealWithErr :: (Either SomeException e -> ExceptT e IO (Either e a))
              -> ExceptT e IO a -> ExceptT e IO a
  dealWithErr f = mapExceptT (`catch` (fmap join . runExceptT . f . fromError))
  {-# INLINE dealWithErr #-}

-- | Handle errors in a @StateT@ monad. When an error is caught, the state is
-- restored to immediately before the action that caused the error. However,
-- the handler may change the state as well, although it is recommended to not
-- do so.
instance MonadErrHandling m => MonadErrHandling (StateT s m) where
  type Err (StateT s m) = Err m

  dealWithErr :: (Either SomeException (Err m) -> StateT s m (Either (Err m) a))
              -> StateT s m a -> StateT s m a
  dealWithErr f stateIO = StateT
    $ \s -> dealWithErr (worker s) (runStateT stateIO s)
    where
      worker s e = do
        (result, s') <- runStateT (f e) s
        return $ second (, s') result
  {-# INLINE dealWithErr #-}

-- | Benign errors are handled using the given function. Serious errors are
-- rethrown as a pure @ExceptT@.
handleErr :: FromError (Err m) => MonadErrHandling m
          => (Err m -> m a) -> m a -> m a
handleErr f = dealWithErr $ \case
  Left err -> throw err
  Right e  -> if isSerious e then pure (Left e) else Right <$> f e
{-# INLINE handleErr #-}
