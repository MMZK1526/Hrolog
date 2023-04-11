{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Internal exception handling utilities.
--
-- They seem to be quite useful; I might consider exposing them in the future.
--
-- In this module, there are three levels of error fatality, namely fatal,
-- serious, and benign. The type class @FromError@ distinguishes between fatal
-- and non-fatal errors, while the type class @HasSeverity@ distinguishes
-- between serious and benign errors. These classfications will become more
-- evident when we look at the type classes.
module Utility.Exception where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Coerce
import           Data.Kind

-- | A type class that can be converted from a @SomeException@. We could then
-- use this representation of the same type to handle it.
--
-- There are three levels of error fatality:
-- 1. Fatal errors are those that cannot be converted to @e@. Those errors will
-- result in @Left@s from @fromError@. They usually represent non-recoverable
-- errors and are usually handled at the outermost level of the program using
-- the good old @catch@ or @handle@.
-- 2. Non-fatal errors are those that can be converted to @e@. Those errors are
-- potentially recoverable. 
--
-- The distinction between fatal and non-fatal errors is used by the type class
-- @MonadErrHandling@, which is usually used with @FromError@.
class FromError e where
  -- | Convert an exception to the given type.
  fromError :: SomeException -> Either SomeException e

-- | A type class that has a notion of severity. It essentially maps every
-- value to @Bool@.
--
-- It can be used in conjunction with @FromError@. @FromError@ distinguishes
-- between fatal and non-fatal errors, while @HasSeverity@ distinguishes between
-- serious and benign errors among the non-fatal ones.
class HasSeverity e where
  -- | Check if the type (usually reprensenting an error) is serious.
  --
  -- By default, it returns @True@ for all inputs.
  isSerious :: e -> Bool
  isSerious = const True

-- | A type that tags the error type @e@ with extra information @t@. Usually,
-- @e@ would implement @FromError@ and @HasSeverity@, while the extra
-- information @t@ would be provided by the monad that handles the error.
data TaggedErr t e = TaggedErr t e

untag :: TaggedErr t e -> e
untag (TaggedErr _ e) = e

instance (HasSeverity e) => HasSeverity (TaggedErr t e) where
  isSerious :: HasSeverity e => TaggedErr t e -> Bool
  isSerious (TaggedErr _ e) = isSerious e

-- | A type class representing a monad that can handle errors.
class MonadIO m => MonadErrHandling m where
  -- | The monolithic error type that this monad can handle.
  --
  -- This type usually implements @FromError@ so that any (non-fatal) impure
  -- exception can be transformed into this type and then be handled. However,
  -- this is not required. In fact, it may contain some extra information
  -- provided by the monad itself.
  type Err m

  -- | Handle all impure exceptions, either rethrow them or catch them properly.
  --
  -- It takes a function that takes an error and returns an @Either@ of a
  -- transformed error of the same type (corresponding to uncaught) or a new
  -- value (corresponding to caught).
  --
  -- If @Err m@ implements @FromError@, The input @Either@ would represent
  -- either a non-fatal error already converted into @Err m@ (@Right@) or a
  -- fatal error that remains in its @SomeException@ form (@Left@).
  --
  -- The function itself does not mind handling a fatal error; it can always
  -- choose to catch it, but it is recommended to rethrow it in most cases.
  dealWithErr :: (Either SomeException (Err m) -> m (Either (Err m) a))
              -> m a -> m a

  -- | Rethrow all non-fatal errors into a pure @FromError@. It does not care
  -- about whether they are benign or not.
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
  dealWithErr f action
    = mapExceptT (`catch` (fmap join . runExceptT . f . fromError)) $ do
      result <- lift $ runExceptT action
      case result of
        Left e  -> f (Right e) >>= except
        Right a -> pure a
  {-# INLINE dealWithErr #-}

-- | Handle errors in a @StateT@ monad. When an error is caught, it records
-- the error AND the state before the action that caused the error.
--
-- When an error is caught, the state is restored to immediately before the
-- action that caused the error. However, the handler may change the state as
-- well, for example storing the error in the state for future reference.
instance MonadErrHandling m => MonadErrHandling (StateT s m) where
  type Err (StateT s m) = TaggedErr s (Err m)

  dealWithErr :: ( Either SomeException (TaggedErr s (Err m))
                -> StateT s m (Either (TaggedErr s (Err m)) a) )
              -> StateT s m a -> StateT s m a
  dealWithErr f stateIO = StateT
    $ \s -> dealWithErr (worker s) (runStateT stateIO s)
    where
      worker s e = do
        (result, s') <- runStateT (f $ second (TaggedErr s) e) s
        return $ bimap untag (, s') result
  {-# INLINE dealWithErr #-}

-- | Benign errors are handled using the given function; serious errors are
-- rethrown as a pure @ExceptT@ with the @FromError@ type; fatal errors are
-- rethrown as impure @Exception@s.
--
-- It is a special case of @dealWithErr@ which uses the semantic meaning of
-- the three levels of error fatality.
handleErr :: HasSeverity (Err m) => MonadErrHandling m
          => (Err m -> m a) -> m a -> m a
handleErr f = dealWithErr $ \case
  Left err -> throw err
  Right e  -> if isSerious e then pure (Left e) else Right <$> f e
{-# INLINE handleErr #-}

-- | The most basic representation of an error, namely a simple @String@.
--
-- It takes two type level List to indicate which error types are benign and
-- which are serious. Everything else are considered fatal.
--
-- In the @StringErr@ constructor, the first @Bool@ is used to indicate whether
-- the error is serious (if @True@) or benign (if @False@). It does not store
-- fatal errors as they are not even able to be converted into the @StringErr@.
-- The second @String@ is used to store the actual error message as a @String@.
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

instance HasSeverity (StringErr benign serious) where
  isSerious :: StringErr benign serious -> Bool
  isSerious (StringErr b _) = b
  {-# INLINE isSerious #-}

-- | An instance where all exceptions are fatal.
instance FromError (StringErr '[] '[]) where
  fromError :: SomeException -> Either SomeException (StringErr '[] '[])
  fromError = Left
  {-# INLINE fromError #-}

-- | An instance where there are no benign errors. We recurse on the type of
-- the serious errors.
instance (FromError (StringErr '[] ss), Exception s)
  => FromError (StringErr '[] (s ': ss)) where
    fromError :: SomeException
              -> Either SomeException (StringErr '[] (s ': ss))
    fromError e = case fromException e :: Maybe s of
      Just t  -> Right $ StringErr True $ show t
      Nothing ->
        coerce (fromError e :: Either SomeException (StringErr '[] (s ': ss)))
    {-# INLINE fromError #-}

-- | We recurse on the type of the benign errors.
instance (FromError (StringErr bs '[ss]), Exception b)
  => FromError (StringErr (b ': bs) '[ss]) where
    fromError :: SomeException
              -> Either SomeException (StringErr (b ': bs) '[ss])
    fromError e = case fromException e :: Maybe b of
      Just t  -> Right $ StringErr True $ show t
      Nothing ->
        coerce (fromError e :: Either SomeException (StringErr (b ': bs) '[ss]))
    {-# INLINE fromError #-}
