{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Internal exception handling utilities.
--
-- They seem to be quite useful; I might consider exposing them in the future.
module Utility.Exception where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Bifunctor

-- | A type class that can be converted from and to a @SomeException@. We could
-- then use this representation of the same type to handle it.
--
-- We usually do not implement this type class directly, but instead use the
-- @FromError@ type class.
class IsoError e where
  -- | Convert an exception to the given type.
  fromError :: SomeException -> e

  -- | Convert the given type to an exception.
  toError :: e -> SomeException

  -- | The free @Iso'@ from @FromError@.
  feIso :: Iso' SomeException e
  feIso = iso fromError toError
  {-# INLINE feIso #-}

-- | A type class that can be converted from some @SomeException@. It is
-- usually used with @TaggedError@ to construct an @IsoError@ instance. See
-- @TaggedError@ for more details.
class FromSomeError e where
  -- | Convert some exception to the given type.
  fromSomeError :: SomeException -> Maybe e

-- | A type class that has a notion of severity. It essentially maps every
-- value to @Bool@.
class HasSeverity e where
  -- | Check if the type (usually reprensenting an error) is serious.
  --
  -- By default, it returns @True@ for all inputs.
  isSerious :: e -> Bool
  isSerious = const True

-- | A type class representing a monad that can handle errors.
--
-- @e@ is the monolithic error type that this monad can handle. It implements
-- @IsoError@ so that any exception, both pure and impure, can be transformed
-- into this type and then be handled.
class (MonadIO m, MonadThrow m, MonadCatch m, IsoError e)
  => MonadErrHandling e m where
    -- | Handle all exceptions, either rethrow them or catch them properly.
    dealWithErr :: (e -> m (Either e a)) -> m a -> m a

    -- | How to throw the error. Depending on the implementation, it may throw
    -- it as an impure exception or wrap it in a pure value.
    throwErr :: e -> m a

-- | Basic @MonadErrHandling@ instance for @IO@. Since it cannot deal with pure
-- errors, it throws everything that comes from the @Left@ in @dealWithErr@.
instance IsoError e => MonadErrHandling e IO where
  dealWithErr :: (e -> IO (Either e a)) -> IO a -> IO a
  dealWithErr f action = (action `catch`) $ \err -> do
    result <- f (fromError err)
    case result of
      Left e  -> throwErr e
      Right a -> pure a
  
  throwErr :: e -> IO a
  throwErr = throwM . toError
  {-# INLINE throwErr #-}

-- | Handle errors in a @EitherT@ monad. It wraps all impure exceptions in
-- a pure @Left@.
instance (IsoError e, MonadErrHandling e m)
  => MonadErrHandling e (ExceptT e m) where
    dealWithErr :: IsoError e => MonadErrHandling e m
                => (e -> ExceptT e m (Either e a))
                -> ExceptT e m a -> ExceptT e m a
    dealWithErr f action
      = mapExceptT (`catch` (fmap join . runExceptT . f . fromError)) $ do
        result <- lift $ runExceptT action
        case result of
          Left e  -> f e >>= except
          Right a -> pure a

    throwErr :: e -> ExceptT e m a
    throwErr = throwE
    {-# INLINE throwErr #-}

-- | Handle errors in a @StateT@ monad. When an error is caught, it records
-- the error AND the state before the action that caused the error.
--
-- When an error is caught, the state is restored to immediately before the
-- action that caused the error. However, the handler may change the state as
-- well, for example storing the error in the state for future reference.
--
-- The behaviour for @throwErr@ matches that of @m@. In other words, it wraps
-- the error as a pure expression if and only if @m@ does so.
instance MonadErrHandling e m => MonadErrHandling e (StateT s m) where
  dealWithErr :: (e -> StateT s m (Either e a))
              -> StateT s m a -> StateT s m a
  dealWithErr f stateIO = (`catch` handler) . StateT
                        $ \s -> dealWithErr (worker s) (runStateT stateIO s)
    where
      handler err = do
        result <- f (fromError err)
        case result of
          Left e  -> throwErr e
          Right a -> pure a
      worker s e = do
        (result, s') <- runStateT (f e) s
        return $ second (, s') result

  throwErr :: e -> StateT s m a
  throwErr = lift . throwErr
  {-# INLINE throwErr #-}

-- It is a special case of @dealWithErr@ which uses the semantic meaning of
-- the levels of error severity. Serious errors are rethrown, while benign
-- errors are handled by the given handler.
handleErr :: HasSeverity e => MonadErrHandling e m => (e -> m a) -> m a -> m a
handleErr f = dealWithErr $
  \e -> if isSerious e then pure $ Left e else Right <$> f e
{-# INLINE handleErr #-}

-- | A type class that is isomorphic to @SomeException@ if we can convert
-- @SomeException@ to @Maybe a@.
--
-- @a@ usually implements @FromSomeError@ and @HasSeverity@, in this case,
-- there are three level of error severity:
-- 1. Fatal errors: They corresponds to @TaggedError e Nothing@.
-- 2. Serious errors: They corresponds to @TaggedError e (Just a)@, but
-- @isSerious a@ returns @True@.
-- 3. Benign errors: They corresponds to @TaggedError e (Just a)@, and
-- @isSerious a@ returns @False@.
data TaggedError a = forall e. Exception e => TaggedError e (Maybe a)

instance (FromSomeError e) => IsoError (TaggedError e) where
  fromError :: SomeException -> TaggedError e
  fromError err = case fromException err of
    Just e  -> TaggedError e (fromSomeError e)
    Nothing -> TaggedError err Nothing
  
  toError :: TaggedError e -> SomeException
  toError (TaggedError e _) = toException e

instance (HasSeverity e) => HasSeverity (TaggedError e) where
  isSerious :: TaggedError e -> Bool
  isSerious (TaggedError _ Nothing)  = True
  isSerious (TaggedError _ (Just e)) = isSerious e
