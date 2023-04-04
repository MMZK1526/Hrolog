module Parser where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Program
import           Text.Megaparsec (Parsec, ParsecT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error
import qualified Data.Set as S
import           Data.Void

type ParserT = ParsecT Void String
type Parser  = Parsec Void String

-- | Run the parser combinator for Hrolog programs.
parseProgram :: String -> Either String Program
parseProgram str = evalState (parseT program str) emptyProgram

-- | Run the parser combinator for Hrolog queries.
parsePQuery :: String -> Either String PQuery
parsePQuery str = evalState (parseT pQuery str) emptyProgram

-- | Generic parse function.
parseT :: Monad m
       => ParserT m a -> String -> m (Either String a)
parseT parser str = left errorBundlePretty
                <$> P.runParserT (space >> parser <* P.eof) "Hrolog" str

-- | Generic parse function without inner Monad.
parse :: Parser a -> String -> Either String a
parse = (runIdentity .) . parseT

-- | Parse spaces.
space :: Monad m => ParserT m ()
space = L.space P.space1 (L.skipLineComment "#") P.empty

-- | Parse a character with space after it.
char :: Monad m => Char -> ParserT m Char
char = L.lexeme space . P.char

-- | Parse a string with space after it.
string :: Monad m => String -> ParserT m String
string = L.lexeme space . P.string

-- | Parse an identifier (a string of letters and digits, starting with a
-- lowercase letter or a digit), with space after it.
identifier :: Monad m => ParserT m String
identifier = L.lexeme space
           $ liftM2 (:) (P.lowerChar P.<|> P.digitChar)
                        (P.many (P.alphaNumChar P.<?> "identifier"))

-- | Parse a variable (a string of letters and digits, starting with an
-- uppercase letter), with space after it.
variable :: Monad m => ParserT (StateT Program m) String
variable = do
  v <- L.lexeme space
     $ liftM2 (:) P.upperChar (P.many (P.alphaNumChar P.<?> "variable"))
  lift $ modify' (variables %~ S.insert v)
  return v

-- | Parse a @Constant@, which is the same as an identifier, with space after
-- it.
constant :: Monad m => ParserT (StateT Program m) Constant
constant = do
  name <- identifier
  let c = Constant name
  lift $ modify' (constants %~ S.insert c)
  return c

-- | Parse a @Term@, which is either a @Constant@ or a @Variable@.
term :: Monad m => ParserT (StateT Program m) Term
term = P.choice [ ConstantTerm <$> constant
                , VariableTerm <$> variable ] P.<?> "term"

-- | Parse an @Atom@, which is a @Predicate@ followed by a list of @Term@s.
atom :: Monad m => ParserT (StateT Program m) Atom
atom = do
  name <- identifier
  ts   <- P.option []
                   (P.between (char '(') (char ')') (P.sepBy term (char ',')))
  let pd = Predicate name (length ts)
  lift $ modify (predicates %~ S.insert pd)
  return $ Atom pd ts

-- | Parse a @Clause@, which is an optional @Atom@ head followed by a list of
-- @Atom@ bodies.
clause :: Monad m => ParserT (StateT Program m) Clause
clause = liftM2 Clause
                (P.optional atom)
                (P.option [] (string "<-" >> P.sepBy atom (char ',')))
      <* char '.'

-- | Parse the whole Hrolog program.
program :: Monad m => ParserT (StateT Program m) Program
program = do
  cs <- P.many clause
  p  <- lift get
  return $ p & clauses .~ cs

-- | Parse a Hrolog query.
pQuery :: Monad m => ParserT (StateT Program m) PQuery
pQuery = do
  as <- P.sepBy atom (char ',') <* char '.'
  vs <- lift $ gets _variables
  return $ PQuery vs as
