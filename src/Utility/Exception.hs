{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utility.Exception where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Proxy

-- | A type class that can be converted from an Exception.
class FromError a where
  -- | Convert an exception to the given type.
  fromError :: SomeException -> a

  -- | Check if the given exception is fatal.
  --
  -- By default, all exceptions are fatal.
  isFatal :: Proxy a -> SomeException -> Bool
  isFatal = const (const True)

  -- | Handle the given error.
  handleAction :: a -> IO ()

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
  
  handleAction :: StringErr -> IO ()
  handleAction (StringErr s) = putStrLn s

-- | Handle any @IOException@ by simply recording them as @String@s, and rethrow
-- other exceptions as a pure @ExceptT@, in a @StateT@ context.
--
-- If an error is caught, the state will not be updated.
handleStateErr :: FromError e
               => StateT s (ExceptT e IO) () -> StateT s (ExceptT e IO) ()
handleStateErr stateIO = StateT $ \s -> ExceptT $ do
  result <- runExceptT . handleErr ((), s) $ runStateT stateIO s
  return $ case result of
    Left err -> Left err
    Right as -> pure as

-- | Non-fatal errors and handled using the @handleAction@ method of the
-- @FromError@ instance. Fatal errors are rethrown as a pure @ExceptT@.
handleErr :: forall e a. FromError e => a -> ExceptT e IO a -> ExceptT e IO a
handleErr a = mapExceptT (`catch` handler)
  where
    handler (e :: SomeException)
      | isFatal (Proxy :: Proxy e) e = pure (Left (fromError e))
      | otherwise                    = Right a <$ handleAction (fromError e :: e)
