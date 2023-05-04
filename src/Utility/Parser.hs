-- | This module contains most of the parser combinators used in the project.
module Utility.Parser where

import           Control.Arrow
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           Text.Megaparsec (Parsec, ParsecT)
import qualified Text.Megaparsec as P
import           Text.Megaparsec.Error

type ParserT = ParsecT Void Text
type Parser  = Parsec Void Text

-- | Generic parse function.
parseT :: Monad m
       => ParserT m b -> ParserT m a -> Text -> m (Either Text a)
parseT spaceP parser str = left (T.pack . errorBundlePretty)
                <$> P.runParserT (spaceP >> parser <* P.eof) "Hrolog" str
{-# INLINE parseT #-}

-- | Generic parse function without inner Monad.
parse :: Parser b -> Parser a -> Text -> Either Text a
parse  = ((runIdentity .) .) . parseT
{-# INLINE parse #-}
