{-# LANGUAGE ScopedTypeVariables #-}

module Utility.Exception where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           System.IO.Error
import           System.Directory

-- | Handle any @IOError@ by simply recording them as @String@s, and rethrow
-- other exceptions as a pure @ExceptT@, in a @StateT@ context.
--
-- If an error is caught, the state will not be updated.
handleStateErr :: StateT s (ExceptT String IO) () -> StateT s (ExceptT String IO) ()
handleStateErr stateIO = StateT $ \s -> ExceptT $ do
  result <- runExceptT . handleErr ((), s) $ runStateT stateIO s
  return $ case result of
    Left err -> Left err
    Right as -> pure as

-- | Handle any @IOError@ by simply recording them as @String@s, and rethrow
-- other exceptions as a pure @ExceptT@.
handleErr :: a -> ExceptT String IO a -> ExceptT String IO a
handleErr a = mapExceptT (`catches` [Handler logIOErrors, Handler rethrowOtherErrors])
  where
    logIOErrors (e :: IOError)
      | isDoesNotExistError e = do
        curDir <- liftIO getCurrentDirectory
        let fileDNEErrMsg = case ioeGetFileName e of
              Just fn -> concat ["File ", show fn, " does not exist."]
              _       -> "File does not exist."
        let curDirMsg     = concat ["Current directory is ", show curDir, "."]
        putStrLn $ unlines [fileDNEErrMsg, curDirMsg]
        return $ Right a
      | otherwise             = Right a <$ putStrLn ("IO Error:\n" ++ show e)
    rethrowOtherErrors (e :: SomeException) = pure (Left ("Fatal Exception:\n" ++ show e))
