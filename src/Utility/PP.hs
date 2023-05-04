{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module defines a internal pretty printing class that supports various
-- verbosity levels. It is used by various data structures defined in this
-- library so that their original @Show@ instances can be used for debugging
-- purposes.
module Utility.PP where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Pretty printing options.
data PPOp = Verbose | Succinct

-- | Pretty printing class. The first parameter is the pretty printing option.
-- Used as an alternative to the @Show@ class.
--
-- This type class is internal, and types with exposed pretty prints has their
-- own public functions.
class PP p a where
  pShowF :: p -> a -> Text

  pPrintF :: p -> a -> IO ()
  pPrintF = (T.putStrLn .) . pShowF
  {-# INLINE pPrintF #-}

  pPrintF' :: p -> a -> IO ()
  pPrintF' = (T.putStr .) . pShowF
  {-# INLINE pPrintF' #-}

pShow :: PP () a => a -> Text
pShow = pShowF ()
{-# INLINE pShow #-}

pPrint :: PP () a => a -> IO ()
pPrint = pPrintF ()
{-# INLINE pPrint #-}

pPrint' :: PP () a => a -> IO ()
pPrint' = pPrintF' ()
{-# INLINE pPrint' #-}

instance PP () Text where
  pShowF :: () -> Text -> Text
  pShowF = const id

instance PP () String where
  pShowF :: () -> String -> Text
  pShowF = const T.pack

instance PP () Int where
  pShowF :: () -> Int -> Text
  pShowF = const $ T.pack . show
