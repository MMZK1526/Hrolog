{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Hrolog.Builder where

import           Control.Monad.Trans.Writer.CPS
import           Data.String
import           Data.Text (Text)

import           Internal.Program
import           Utility.PP
import Data.Functor

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

peanoNumbers :: Program
peanoNumbers = program do
  fact_ $ atom_ "add" do
    lit_ "0"
    lit_ "Y"
    lit_ "Y"
  atom_ "add" (term_ "s" (lit_ "X") <> lit_ "Y" <> term_ "s" (lit_ "Z"))
    <-| atom_ "add" do
      lit_ "X"
      lit_ "Y"
      lit_ "Z"
  atom_ "sub" (lit_ "X" <> lit_ "Y" <> lit_ "Z")
    <-| atom_ "add" do
      lit_ "Y"
      lit_ "Z"
      lit_ "X"
  fact_ $ atom_ "fib" do
    lit_ "0"
    lit_ "0"
  fact_ $ atom_ "fib" do
    term_ "s" (lit_ "0")
    term_ "s" (lit_ "0")
  atom_ "fib" (term_ "s" (term_ "s" (lit_ "Y")))
    <-| do
      atom_ "fib" do
        lit_ "X"
        lit_ "A"
      atom_ "fib" do
        term_ "s" (lit_ "X")
        lit_ "B"
      atom_ "add" do
        lit_ "A"
        lit_ "B"
        lit_ "Y"
