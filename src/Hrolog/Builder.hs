{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

-- | A type-safe DSL for building Hrolog programs.
--
-- This module contains many internal implementation details. Only the functions
-- in the export list are intended to be used by end users. While these
-- functions are documented, it may not be the most intuitive to start by
-- reading them. Instead, it may be more helpful to start with examples in
-- "Hrolog.Builder.Examples".
module Hrolog.Builder (
  Builder,
  renderBuilder,
  runBuilder,
  run,
  lit_,
  func_,
  func,
  atom_,
  atom,
  fact_,
  fact,
  rule_,
  rule,
  (<-|),
  constraint_,
  constraint,
  program_,
  program,
) where

import           Control.Monad.Trans.Writer.CPS
import           Data.DList (DList)
import qualified Data.DList as D
import           Data.String
import           Data.Text (Text)
import           GHC.Exts

import           Internal.Program
import           Utility.PP


--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

data Single

class HasPlurality a

instance HasPlurality Single
instance HasPlurality ()

newtype Builder w t a = Builder { unBuilder :: Writer (DList w) a }
  deriving newtype Functor

instance Applicative (Builder w ()) where
  pure :: a -> Builder w () a
  pure = Builder . pure

  (<*>) :: Builder w () (a -> b) -> Builder w () a -> Builder w () b
  (<*>) (Builder f) (Builder a) = Builder $ f <*> a

instance Monad (Builder w ()) where
  (>>=) :: Builder w () a -> (a -> Builder w () b) -> Builder w () b
  (>>=) (Builder m) f = Builder $ m >>= unBuilder . f

instance (IsString s, HasPlurality t) => IsString (Builder s t ()) where
  fromString :: String -> Builder s t ()
  fromString str = Builder $ do
    tell . pure $ fromString str

instance IsList (Builder w () ()) where
  type Item (Builder w () ()) = Builder w () ()

  fromList :: [Builder w () ()] -> Builder w () ()
  fromList = Builder . mapM_ unBuilder

  toList :: Builder w t () -> [Builder w t ()]
  toList = pure

instance Show s => Show (Builder s Single ()) where
  show :: Show s => Builder s Single () -> String
  show = show . run

renderBuilder :: PP () s => Builder s Single () -> Text
renderBuilder = pShow . run
{-# INLINE renderBuilder #-}

instance Semigroup (Builder w () ()) where
  (<>) :: Builder w () () -> Builder w () () -> Builder w () ()
  (<>) = (>>)

instance Monoid (Builder w () ()) where
  mempty :: Builder w () ()
  mempty = pure ()


--------------------------------------------------------------------------------
-- Conbinators
--------------------------------------------------------------------------------

runBuilder :: Builder w t a -> [w]
runBuilder = D.toList . execWriter . unBuilder
{-# INLINE runBuilder #-}

run :: Builder w Single () -> w
run = head . runBuilder
{-# INLINE run #-}

lit_ :: HasPlurality t => w -> Builder w t ()
lit_ = Builder . tell . pure
{-# INLINE lit_ #-}

func_ :: HasPlurality t => Text -> Builder Term () () -> Builder Term t ()
func_ name body = Builder . tell $ D.singleton (mkFTerm' name (runBuilder body))
{-# INLINE func_ #-}

func :: Text -> Builder Term () () -> Term
func = (run .) . func_
{-# INLINE func #-}

atom_ :: HasPlurality t => Text -> Builder Term () () -> Builder Atom t ()
atom_ name body = Builder . tell $ D.singleton (mkAtom name (runBuilder body))
{-# INLINE atom_ #-}

atom :: Text -> Builder Term () () -> Atom
atom = (run .) . atom_
{-# INLINE atom #-}

rule_ :: HasPlurality t
      => Builder Atom Single () -> Builder Atom () () -> Builder Clause t ()
rule_ h b = Builder . tell $ D.singleton (run h :<- runBuilder b)
{-# INLINE rule_ #-}

rule :: Builder Atom Single () -> Builder Atom () () -> Clause
rule = (run .) . rule_
{-# INLINE rule #-}

(<-|) :: HasPlurality t
      => Builder Atom Single () -> Builder Atom () () -> Builder Clause t ()
(<-|) = rule_
{-# INLINE (<-|) #-}

fact_ :: HasPlurality t => Builder Atom Single () -> Builder Clause t ()
fact_ h = Builder . tell $ D.singleton (run h :<- [])
{-# INLINE fact_ #-}

fact :: Builder Atom Single () -> Clause
fact = run . fact_
{-# INLINE fact #-}

constraint_ :: HasPlurality t => Builder Atom () () -> Builder Clause t ()
constraint_ b = Builder . tell $ D.singleton (Constraint $ runBuilder b)
{-# INLINE constraint_ #-}

constraint :: Builder Atom () () -> Clause
constraint = run . constraint_
{-# INLINE constraint #-}

program_ :: HasPlurality t => Builder Clause () () -> Builder Program t ()
program_ b = Builder . tell $ D.singleton (mkProgram $ runBuilder b)
{-# INLINE program_ #-}

program :: Builder Clause () () -> Program
program = run . program_
{-# INLINE program #-}
