-- | Parsers for Hrolog programs and queries.
module Parser where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Text.Megaparsec (Parsec, ParsecT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error
import qualified Data.Set as S
import           Data.Void

import           Internal.Program

type ParserT = ParsecT Void String
type Parser  = Parsec Void String

-- | Run the parser combinator for Hrolog programs.
parseProgram :: String -> Either String Program
parseProgram str = right runIdentity
                 $ evalState (parseT program str) (Identity emptyProgram)
{-# INLINE parseProgram #-}

-- -- | Run the parser combinator for Hrolog queries.
parsePQuery :: String -> Either String PQuery
parsePQuery str = right runIdentity
                $ evalState (parseT pQuery str) (Identity emptyProgram)
{-# INLINE parsePQuery #-}

-- | Generic parse function.
parseT :: Monad m
       => ParserT m a -> String -> m (Either String a)
parseT parser str = left errorBundlePretty
                <$> P.runParserT (space >> parser <* P.eof) "Hrolog" str
{-# INLINE parseT #-}

-- | Generic parse function without inner Monad.
parse :: Parser a -> String -> Either String a
parse = (runIdentity .) . parseT
{-# INLINE parse #-}

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
--
-- Here we need a "Program" because we need to keep track of the variables in
-- the "Program" to be built. The "Program" is wrapped within a "Functor" so
-- that we can keep track of an arbitrary number of "Program"s (including none,
-- in which we do not update any state in the "Program" at all).
variable :: Functor f => Monad m => ParserT (StateT (f Program) m) String
variable = do
  v <- L.lexeme space
     $ liftM2 (:) P.upperChar (P.many (P.alphaNumChar P.<?> "variable"))
  lift $ modify' (fmap (variables %~ S.insert v))
  return v

-- | Parse a @Constant@, which is the same as an identifier, with space after
-- it.
constant :: Functor f => Monad m => ParserT (StateT (f Program) m) Constant
constant = do
  name <- identifier
  let c = Constant name
  lift $ modify' (fmap (constants %~ S.insert c))
  return c

-- | Parse a @Term@, which is either a @Constant@ or a @Variable@.
term :: Functor f => Monad m => ParserT (StateT (f Program) m) Term
term = P.choice [ ConstantTerm <$> constant
                , VariableTerm <$> variable ] P.<?> "term"

-- | Parse an @Atom@, which is a @Predicate@ followed by a list of @Term@s.
atom :: Functor f => Monad m => ParserT (StateT (f Program) m) Atom
atom = do
  name <- identifier
  ts   <- P.option [] (P.between (char '(') (char ')') (P.sepBy term (char ',')))
  let pd = Predicate name (length ts)
  lift $ modify (fmap (predicates %~ S.insert pd))
  return $ Atom pd ts

-- | Parse a @Clause@, which is an optional @Atom@ head followed by a list of
-- @Atom@ bodies.
clause :: Functor f => Monad m => ParserT (StateT (f Program) m) Clause
clause = liftM2 Clause
                (P.optional atom)
                (P.option [] (string "<-" >> P.sepBy atom (char ',')))
      <* char '.'

-- | Parse the whole Hrolog program.
program :: Functor f => Monad m => ParserT (StateT (f Program) m) (f Program)
program = do
  cs <- P.many clause
  p  <- lift get
  return $ fmap (clauses .~ cs) p

-- | Parse a Hrolog query.
--
-- The @Program@'s variables are cleared before parsing the query so that it
-- represents the variables in the query by the end.
--
-- TODO: Use the @Program@'s predicates to check that the query is valid.
pQuery :: Functor f => Monad m => ParserT (StateT (f Program) m) (f PQuery)
pQuery = do
  lift $ modify' (fmap (variables .~ S.empty)) -- Clear variables in the program
  as <- P.sepBy atom (char ',') <* char '.'
  -- The variables are the ones in the query since we cleared the variables in
  -- the program.
  vs <- lift $ fmap (view variables) <$> get
  return $ (`PQuery` as) <$> vs
