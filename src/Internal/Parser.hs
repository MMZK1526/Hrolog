-- | Internal parsers for Hrolog programs and queries.
module Internal.Parser (module Internal.Parser, module Utility.Parser) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S

import           Internal.Program
import           Utility.Parser
import           Utility.PP

-- | Run the parser combinator for Hrolog programs.
parseProgram :: String -> Either String Program
parseProgram str = right runIdentity
                 $ evalState (parseT space program str) (Identity emptyProgram)
{-# INLINE parseProgram #-}

-- | Run the parser combinator for Hrolog queries.
--
-- If a @Program@ is provided, the parser will check if the predicates and
-- functions used in the query are defined in the @Program@. Otherwise, the
-- parser will not check the semantics of the query.
parsePQuery :: Maybe Program -> String -> Either String PQuery
parsePQuery mProgram str = case mProgram of
  Nothing -> right runIdentity
           $ evalState (parseT space pQuery str) (Identity emptyProgram)
  Just p  -> right runIdentity
           $ evalState (parseT space (pQuery' True) str) (Identity p)
{-# INLINE parsePQuery #-}

-- | Parse spaces, ignoring comments.
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
-- the "Program" to be built. The "Program" is wrapped within a "Traversable" so
-- that we can keep track of an arbitrary number of "Program"s (including none,
-- in which we do not update any state in the "Program" at all).
variable :: Traversable f => Monad m => ParserT (StateT (f Program) m) String
variable = do
  v <- L.lexeme space
     $ liftM2 (:) P.upperChar (P.many (P.alphaNumChar P.<?> "variable"))
  lift $ modify' (fmap (variables %~ S.insert v))
  pure v

-- | Parse a "FunctionTerm".
functionTerm :: Traversable f => Monad m
             => ParserT (StateT (f Program) m) FunctionTerm
functionTerm = functionTerm' False
{-# INLINE functionTerm #-}

functionTerm' :: Traversable f => Monad m
              => Bool -> ParserT (StateT (f Program) m) FunctionTerm
functionTerm' isQuery = do
  name <- identifier
  ts   <- P.option []
                   (P.between (char '(') (char ')') (P.sepBy term (char ',')))
  let fd = Function name (length ts)
  if isQuery
    then do
      fz <- fmap (view functions) <$> lift get
      forM_ fz $ \fs -> unless (fd `S.member` fs) $ if null ts
        then fail $ concat ["Undefined constant ", pShow fd, "!"]
        else fail $ concat ["Undefined function ", name, "!"]
    else lift $ modify' (fmap (functions %~ S.insert fd))
  pure $ FTerm fd ts

-- | Parse a @Term@, which is either a @Constant@ or a @Variable@.
term :: Traversable f => Monad m => ParserT (StateT (f Program) m) Term
term = term' False
{-# INLINE term #-}

term' :: Traversable f => Monad m => Bool -> ParserT (StateT (f Program) m) Term
term' isQuery = P.choice [ VariableTerm <$> variable
                         , FunctionTerm <$> functionTerm' isQuery ]
            P.<?> "term"

-- | Parse an @Atom@, which is a @Predicate@ followed by a list of @Term@s.
atom :: Traversable f => Monad m => ParserT (StateT (f Program) m) Atom
atom = atom' False
{-# INLINE atom #-}


atom' :: Traversable f => Monad m => Bool -> ParserT (StateT (f Program) m) Atom
atom' isQuery = do
  name <- identifier
  ts   <- P.option [] $
    P.between (char '(') (char ')') (P.sepBy (term' isQuery) (char ','))
  let pd = Predicate name (length ts)
  if isQuery
    then do
      pz <- fmap (view predicates) <$> lift get
      forM_ pz $ \ps ->
        unless (pd `S.member` ps) $ fail $ if null ts
        then fail $ concat ["Undefined predicate ", pShow pd, "!"]
        else fail $ concat ["Undefined literal ", name, "!"]
    else
      lift $ modify (fmap (predicates %~ S.insert pd))
  pure $ Atom pd ts

-- | Parse a @Clause@, which is an optional @Atom@ head followed by a list of
-- @Atom@ bodies.
clause :: Traversable f => Monad m => ParserT (StateT (f Program) m) Clause
clause = liftM2 Clause
                (P.optional atom)
                (P.option [] (string "<-" >> P.sepBy atom (char ',')))
      <* char '.'

-- | Parse the whole Hrolog program.
program :: Traversable f => Monad m
        => ParserT (StateT (f Program) m) (f Program)
program = do
  cs <- P.many clause
  p  <- lift get
  pure $ fmap (clauses .~ cs) p

-- | Parse a Hrolog query.
--
-- The @Program@'s variables are cleared before parsing the query so that it
-- represents the variables in the query by the end.
pQuery :: Traversable f => Monad m => ParserT (StateT (f Program) m) (f PQuery)
pQuery = pQuery' False
{-# INLINE pQuery #-}

pQuery' :: Traversable f => Monad m => Bool
        -> ParserT (StateT (f Program) m) (f PQuery)
pQuery' checkSemantics = do
  void $ P.optional (string "<-") -- Allow the "<-", but can live without it
  lift $ modify' (fmap (variables .~ S.empty)) -- Clear variables in the program
  as <- P.sepBy (atom' checkSemantics) (char ',') <* char '.'
  -- The variables are the ones in the query since we cleared the variables in
  -- the program.
  vs <- lift $ fmap (view variables) <$> get
  pure $ (`PQuery` as) <$> vs
