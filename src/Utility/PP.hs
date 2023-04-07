{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module defines a internal pretty printing class that supports various
-- verbosity levels. It is used by various data structures defined in this
-- library so that their original @Show@ instances can be used for debugging
-- purposes.
module Utility.PP where

-- | Pretty printing options.
data PPOp = Verbose | Succinct

-- | Pretty printing class. The first parameter is the pretty printing option.
-- Used as an alternative to the @Show@ class.
--
-- This type class is internal, and types with exposed pretty prints has their
-- own public functions.
class PP p a where
  pShowF :: p -> a -> String

  pPrintF :: p -> a -> IO ()
  pPrintF = (putStrLn .) . pShowF
  {-# INLINE pPrintF #-}

  pPrintF' :: p -> a -> IO ()
  pPrintF' = (putStr .) . pShowF
  {-# INLINE pPrintF' #-}

pShow :: PP () a => a -> String
pShow = pShowF ()
{-# INLINE pShow #-}

pPrint :: PP () a => a -> IO ()
pPrint = pPrintF ()
{-# INLINE pPrint #-}

pPrint' :: PP () a => a -> IO ()
pPrint' = pPrintF' ()
{-# INLINE pPrint' #-}

instance PP () String where
  pShowF :: () -> String -> String
  pShowF = const show
