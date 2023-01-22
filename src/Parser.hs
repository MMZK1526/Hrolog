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

space :: Monad m => ParserT m ()
space = L.space P.space1 (L.skipLineComment "#") P.empty

char :: Monad m => Char -> ParserT m Char
char = L.lexeme space . P.char

identifier :: Monad m => ParserT m String
identifier = L.lexeme space $ liftM2 (:) P.lowerChar (P.many P.alphaNumChar)

variable :: Monad m => ParserT m String
variable = L.lexeme space $ liftM2 (:) P.upperChar (P.many P.alphaNumChar)

constant :: Monad m => ParserT (StateT Program m) Constant
constant = do
  name <- identifier
  let c = Constant name
  lift $ modify' (\p -> p { _constants = S.insert c (_constants p) })
  return c

term :: Monad m => ParserT (StateT Program m) Term
term = P.choice [ ConstantTerm <$> constant
                , VariableTerm <$> variable ]

atom :: Monad m => ParserT (StateT Program m) Atom
atom = do
  name <- identifier
  ts   <- P.choice [ P.between (char '(') (char ')') (P.sepBy term (char ','))
                   , pure [] ]
  let pd = Predicate name (length ts)
  lift $ modify' (\p-> p { _predicates = S.insert pd (_predicates p) })
  return $ Atom pd ts
