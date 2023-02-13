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

parseProgram :: String -> Either String Program
parseProgram str = evalState (parseT program str) emptyProgram

parsePQuery :: String -> Either String [Atom]
parsePQuery str = evalState (parseT pQuery str) emptyProgram

parseT :: Monad m
       => ParserT m a -> String -> m (Either String a)
parseT parser str = left errorBundlePretty
                <$> P.runParserT (space >> parser <* P.eof) "Hrolog" str

parse :: Parser a -> String -> Either String a
parse = (runIdentity .) . parseT

space :: Monad m => ParserT m ()
space = L.space P.space1 (L.skipLineComment "#") P.empty

char :: Monad m => Char -> ParserT m Char
char = L.lexeme space . P.char

string :: Monad m => String -> ParserT m String
string = L.lexeme space . P.string

identifier :: Monad m => ParserT m String
identifier = L.lexeme space
           $ liftM2 (:) (P.lowerChar P.<|> P.digitChar)
                        (P.many (P.alphaNumChar P.<?> "identifier"))

variable :: Monad m => ParserT m String
variable = L.lexeme space
         $ liftM2 (:) P.upperChar (P.many (P.alphaNumChar P.<?> "variable"))

constant :: Monad m => ParserT (StateT Program m) Constant
constant = do
  name <- identifier
  let c = Constant name
  lift $ modify' (\p -> p { _constants = S.insert c (_constants p) })
  return c

term :: Monad m => ParserT (StateT Program m) Term
term = P.choice [ ConstantTerm <$> constant
                , VariableTerm <$> variable ] P.<?> "term"

atom :: Monad m => ParserT (StateT Program m) Atom
atom = do
  name <- identifier
  ts   <- P.option []
                   (P.between (char '(') (char ')') (P.sepBy term (char ',')))
  let pd = Predicate name (length ts)
  lift $ modify' (\p -> p { _predicates = S.insert pd (_predicates p) })
  return $ Atom pd ts

clause :: Monad m => ParserT (StateT Program m) Clause
clause = liftM2 Clause
                (P.optional atom)
                (P.option [] (string "<-" >> P.sepBy atom (char ',')))
      <* char '.'

pQuery :: Monad m => ParserT (StateT Program m) [Atom]
pQuery = P.sepBy atom (char ',') <* char '.'

program :: Monad m => ParserT (StateT Program m) Program
program = do
  cs <- P.many clause
  p  <- lift get
  return $ p { _clauses = cs }
