{-# LANGUAGE RecordWildCards #-}

module Parser where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Program
import           Text.Megaparsec (Parsec, ParsecT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error
import           Data.Functor.Identity
import qualified Data.Set as S
import           Data.Void

type ParserT = ParsecT Void String
type Parser  = Parsec Void String

parseT :: Monad m
       => ParserT m a -> String -> m (Either String a)
parseT parser str = left errorBundlePretty <$> P.runParserT parser "Hrolog" str

parse :: Parser a -> String -> Either String a
parse = (runIdentity .) . parseT

spaceLexer :: Monad m => ParserT m ()
spaceLexer = L.space P.space1 (L.skipLineComment "#") P.empty

identifierParser :: Monad m => ParserT m String
identifierParser = L.lexeme spaceLexer
                 $ liftM2 (:) P.lowerChar (P.many P.alphaNumChar)

variableParser :: Monad m => ParserT m String
variableParser = L.lexeme spaceLexer
               $ liftM2 (:) P.upperChar (P.many P.alphaNumChar)

constantParser :: Monad m => ParserT (StateT Program m) Constant
constantParser = do
  name <- identifierParser
  let result = Constant name
  lift $ modify' (\p@Program {..}
    -> p { _constants = S.insert result _constants })
  return result

termParser :: Monad m => ParserT (StateT Program m) Term
termParser = P.choice [ ConstantTerm <$> constantParser
                      , VariableTerm <$> variableParser ]
