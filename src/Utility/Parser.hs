-- | This module contains most of the parser combinators used in the project.
module Utility.Parser where

import           Control.Arrow
import           Data.Functor.Identity
import           Data.Void
import           Text.Megaparsec (Parsec, ParsecT)
import qualified Text.Megaparsec as P
import           Text.Megaparsec.Error

type ParserT = ParsecT Void String
type Parser  = Parsec Void String

-- | Generic parse function.
parseT :: Monad m
       => ParserT m b -> ParserT m a -> String -> m (Either String a)
parseT spaceP parser str = left errorBundlePretty
                <$> P.runParserT (spaceP >> parser <* P.eof) "Hrolog" str
{-# INLINE parseT #-}

-- | Generic parse function without inner Monad.
parse :: Parser b -> Parser a -> String -> Either String a
parse  = ((runIdentity .) .) . parseT
{-# INLINE parse #-}
