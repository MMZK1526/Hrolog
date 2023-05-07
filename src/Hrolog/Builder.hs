{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type-safe DSL for building Hrolog programs.
--
-- This module contains many internal implementation details. Only the functions
-- in the export list are intended to be used by end users. While these
-- functions are documented, it may not be the most intuitive to start by
-- reading them. Instead, it may be more helpful to start with examples in
-- "Hrolog.Builder.Examples".
module Hrolog.Builder where

import           Control.Monad.Trans.Writer.CPS
import           Data.Functor
import           Data.String
import           Data.Text (Text)

import           Internal.Program
import           Utility.PP


--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

data Single = Single

class HasPlurality a where
  mkPlurality :: a

instance HasPlurality Single where
  mkPlurality :: Single
  mkPlurality = Single

instance HasPlurality () where
  mkPlurality :: ()
  mkPlurality = ()

newtype Builder w t a = Builder { unBuilder :: Writer [w] a }
  deriving newtype (Functor, Applicative)

instance Monad (Builder w ()) where
  (>>=) :: Builder w () a -> (a -> Builder w () b) -> Builder w () b
  (>>=) (Builder m) f = Builder $ m >>= unBuilder . f

instance (IsString s, HasPlurality t) => IsString (Builder s t ()) where
  fromString :: String -> Builder s t ()
  fromString str = Builder $ do
    tell . pure $ fromString str
    pure mkPlurality

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

runBuilder :: Builder w t a -> [w]
runBuilder = execWriter . unBuilder
{-# INLINE runBuilder #-}

run :: Builder w Single () -> w
run = head . runBuilder
{-# INLINE run #-}

lit_ :: HasPlurality t => w -> Builder w t ()
lit_ b = Builder $ do
  tell [b]
  pure mkPlurality
{-# INLINE lit_ #-}

term_ :: HasPlurality t => Text -> Builder Term () () -> Builder Term t ()
term_ name body = Builder $ tell [mkFTerm' name (runBuilder body)] $> mkPlurality
{-# INLINE term_ #-}

atom_ :: HasPlurality t => Text -> Builder Term () () -> Builder Atom t ()
atom_ name body = Builder $ tell [mkAtom name (runBuilder body)] $> mkPlurality
{-# INLINE atom_ #-}

rule_ :: HasPlurality t => Builder Atom Single () -> Builder Atom () () -> Builder Clause t ()
rule_ h b = Builder $ tell [run h :<- runBuilder b] $> mkPlurality
{-# INLINE rule_ #-}

(<-|) :: HasPlurality t => Builder Atom Single () -> Builder Atom () () -> Builder Clause t ()
(<-|) = rule_
{-# INLINE (<-|) #-}

fact_ :: HasPlurality t => Builder Atom Single () -> Builder Clause t ()
fact_ h = Builder $ tell [run h :<- []] $> mkPlurality
{-# INLINE fact_ #-}

constraint_ :: HasPlurality t => Builder Atom () () -> Builder Clause t ()
constraint_ b = Builder $ tell [Constraint (runBuilder b)] $> mkPlurality
{-# INLINE constraint_ #-}

program_ :: HasPlurality t => Builder Clause () () -> Builder Program t ()
program_ b = Builder $ tell [mkProgram $ runBuilder b] $> mkPlurality
{-# INLINE program_ #-}

program :: Builder Clause () () -> Program
program = run . program_
{-# INLINE program #-}
