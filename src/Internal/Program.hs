{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines the data types for a Hrolog program as well as its
-- sub-components. It also introduces the pretty printing class and smart
-- constructors for them.
--
-- Hrolog programs resemble Prolog programs, but with some differences. Notably,
-- the head of a clause is optional (which corresponds to constraints in ASP),
-- and the ":-" symbol is replaced by "<-".
--
-- See "src/Test/programs/" for examples of Hrolog programs.
--
-- See "Program" for the exposed API.
module Internal.Program where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T

import           Utility.PP

-- | A Hrolog predicate with its name and arity.
data Predicate = Predicate { _predicateName :: Text, _predicateArity :: Int }
  deriving (Eq, Ord, Show)

-- | A Hrolog function with its name and arity. A constant is treated as a
-- function with arity 0.
data Function = Function { _functionName :: Text, _functionArity :: Int }
  deriving (Eq, Ord, Show)

-- | A Hrolog function term with its name and a list of arguments.
data FunctionTerm' a = FTerm Function [Term' a]
  deriving (Eq, Ord, Show)
type FunctionTerm = FunctionTerm' Text

-- | A Hrolog term, either a function or a variable. The type variable @a@ is
-- used to represent the variable type.
--
-- A constant is considered the same as a function with arity 0.
--
-- In a real program, "a" should always be a @Text@, but it can carry
-- additional metadata during the solving process.
data Term' a = VariableTerm a
             | FunctionTerm (FunctionTerm' a)
  deriving (Eq, Ord, Show)
type Term = Term' Text

-- Note that the terms constructed by the smart constructor below do not
-- necessarily satisfy the requirement for identifiers as if they are parsed
-- from a program. For example, the following would be considered as a constant:
--
-- >>> fromString "a(b)"
-- "a(b)"
--
-- However, it is impossible to write such a variable from a plain-text program
-- since it would be treated as a predicate or function.
instance IsString a => IsString (Term' a) where
  fromString :: String -> Term' a
  fromString s@(c : _) | isUpper c = VariableTerm (fromString s)
  fromString s                     = Constant (fromString s)

-- | The pattern for a constant.
pattern Constant :: Text -> Term' a
pattern Constant s = FunctionTerm (FTerm (Function s 0) [])

-- | The pattern for a function term.
pattern F :: Function -> [Term' a] -> Term' a
pattern F f ts = FunctionTerm (FTerm f ts)

{-# COMPLETE VariableTerm, F #-}

-- | A Hrolog atom, consisting of a predicate and a list of terms as arguments.
data Atom' a = Atom Predicate [Term' a]
  deriving (Eq, Ord, Show)
type Atom = Atom' Text

atomPredicate :: Lens' (Atom' a) Predicate
atomPredicate = lens (\(Atom p _) -> p) (\(Atom _ ts) p -> Atom p ts)

-- | Similarly, the atoms constructed by the smart constructor below do not
-- necessarily satisfy the requirement for identifiers as if they are parsed
-- from a program.
instance IsString Atom where
  fromString :: String -> Atom
  fromString s = Atom (Predicate (fromString s) 0) []

-- | A Hrolog clause, consisting of an optional head and a list of bodies.
--
-- It represents @[H] <- B1, B2, ..., Bn@.
data Clause' a = Clause { _clauseHead :: Maybe (Atom' a)
                        , _clauseBody :: [Atom' a] }
  deriving (Eq, Ord, Show)
type Clause = Clause' Text

-- | The pattern for the empty clause []. It is used to represent failure.
pattern EmptyClause :: Clause
pattern EmptyClause = Clause Nothing []

-- | The pattern for a clause with a head and a list of bodies.
pattern (:<-) :: Atom' a -> [Atom' a] -> Clause' a
pattern h :<- b = Clause (Just h) b

-- | The pattern for a clause with an optional head and a list of bodies.
pattern (:?<-) :: Maybe (Atom' a) -> [Atom' a] -> Clause' a
pattern mH :?<- b = Clause mH b

-- | The pattern for a constraint with no head.
pattern Constraint :: [Atom' a] -> Clause' a
pattern Constraint b = Clause Nothing b

-- | The pattern for a fact with no body.
pattern Fact :: Atom' a -> Clause' a
pattern Fact h <- Clause (Just h) []

{-# COMPLETE (:?<-) #-}
{-# COMPLETE Constraint, (:<-) #-}

-- | The data type for a Hrolog program, consisting of a series of clauses.
-- It also contains the set of predicates, constants, and variables used in the
-- program.
data Program' a = Program { _predicates :: Set Predicate
                          , _variables  :: Set Text
                          , _functions  :: Set Function
                          , _clauses    :: [Clause' a] }
  deriving (Eq, Ord, Show)
type Program = Program' Text
$(makeLenses ''Program')

-- | The data type for a Hrolog query, consisting the list of query atoms.
-- It also contains the set of variables used in the query.
--
-- It represents the query @<- Q1, Q2, ..., Qn@.
data PQuery' a = PQuery { _pqVariables :: Set a
                        , _pqAtoms     :: [Atom' a] }
  deriving (Eq, Ord, Show)
type PQuery = PQuery' Text
$(makeLenses ''PQuery')

-- | The data type for a single Hrolog solution, consisting of a substitution
-- map from variables to terms.
newtype Solution = Solution (Map Text Term)
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
-- PP instances.
--------------------------------------------------------------------------------

instance PP () Predicate where
  pShowF :: () -> Predicate -> Text
  pShowF _ Predicate {..}
    = T.concat [_predicateName, "/", pShow _predicateArity]

instance PP () Function where
  pShowF :: () -> Function -> Text
  pShowF _ Function {..} = T.concat [_functionName, "/", pShow _functionArity]

instance PP () FunctionTerm where
  pShowF :: () -> FunctionTerm -> Text
  pShowF _ (FTerm f []) = _functionName f
  pShowF _ (FTerm f as)
    = T.concat [_functionName f, "(", T.intercalate ", " (pShow <$> as), ")"]

instance PP () Term where
  pShowF :: () -> Term -> Text
  pShowF _ (VariableTerm v) = v
  pShowF _ (F f as)         = pShow (FTerm f as)

instance PP () Atom where
  pShowF :: () -> Atom -> Text
  pShowF _ (Atom p as) = case _predicateArity p of
    0 -> _predicateName p
    _ -> T.concat
      [_predicateName p, "(", T.intercalate ", " (pShow <$> as), ")"]

instance PP () Clause where
  pShowF :: () -> Clause -> Text
  pShowF _ Clause {..} = T.concat [maybe "" pShow _clauseHead, bodyStr, "."]
    where
      bodyStr = case _clauseBody of
        [] -> ""
        cs -> T.concat [ maybe "" (const " ") _clauseHead, "<- "
                       , T.intercalate ", " (pShow <$> cs) ]

instance PP () Program where
  pShowF :: () -> Program -> Text
  pShowF = const $ pShowF Verbose

instance PP PPOp Program where
  pShowF :: PPOp -> Program -> Text
  pShowF vbLvl Program {..} = case vbLvl of
    Succinct -> clStr
    Verbose  -> T.intercalate "\n" [clStr, vsStr, psStr, fsStr]
    where
      clStr = T.unlines (pShow <$> _clauses)
      vsStr = case S.toList _variables of
        [] -> ""
        vs -> T.concat ["Variables: ", T.intercalate ", " vs, ".\n"]
      psStr = case S.toList _predicates of
        [] -> ""
        ps -> T.concat
          ["Predicates: ", T.intercalate ", " (pShow <$> ps), ".\n"]
      fsStr = case S.toList _functions of
        [] -> ""
        fs -> T.concat ["Functions: ", T.intercalate ", " (pShow <$> fs), ".\n"]

instance PP () PQuery where
  pShowF :: () -> PQuery -> Text
  pShowF _ (PQuery _ as) = T.intercalate ", " (pShow <$> as) <> "."

instance PP () Solution where
  pShowF :: () -> Solution -> Text
  pShowF () (Solution sMap)
    | M.null sMap = "Valid\n"
    | otherwise   = T.concat $ map showEntry (M.toAscList sMap)
    where
      showEntry (v, t) = T.concat [v, " = ", pShow t, ";\n"]

-- | Pretty print the program.
prettifyProgram :: Program -> Text
prettifyProgram = pShowF Succinct

-- | Pretty print the solution.
prettifySolution :: Solution -> Text
prettifySolution = pShow


--------------------------------------------------------------------------------
-- Constructors & Helpers.
--------------------------------------------------------------------------------

-- | The empty @Clause@ (failure).
emptyClause :: Clause
emptyClause = EmptyClause

-- | The empty @Program@ (entails nothing).
emptyProgram :: Program
emptyProgram = Program S.empty S.empty S.empty []

-- | A smart constructor to make a function term from a function name and a list
-- of terms. It is only used internally and by the test suite.
mkFTerm :: Text -> [Term] -> FunctionTerm
mkFTerm name ts = FTerm (Function name (length ts)) ts

-- | Similar to @mkFTerm@, but returns a @Term@ instead of a @FunctionTerm@.
mkFTerm' :: Text -> [Term] -> Term
mkFTerm' name ts = F (Function name (length ts)) ts

-- | A smart constructor to make an atom from a predicate name and a list of
-- terms. It is only used internally and by the test suite.
mkAtom :: Text -> [Term] -> Atom
mkAtom name ts = Atom (Predicate name (length ts)) ts

-- | Turn a list of @Clause@s into a @Program@ by calculating the set of
-- predicates, constants, and variables.
--
-- This function is internal and is only used for testing. The user should
-- create a @Program@ by using @parseProgram@ in module Parser.
mkProgram :: [Clause] -> Program
mkProgram cs
  = execState (forM_ cs workClause) (Program S.empty S.empty S.empty cs)
  where
    workClause (Clause mHead body) = do
      forM_ mHead workAtom
      forM_ body workAtom
    workAtom (Atom p ts)           = do
      predicates %= S.insert p
      forM_ ts workTerm
    workTerm (VariableTerm v)      = variables %= S.insert v
    workTerm (F f as)              = do
      functions %= S.insert f
      forM_ as workTerm

-- | Turn a list of @Atom@s into a @PQuery@ by calculating the set of variables.
--
-- This function is internal and is only used for testing. The user should
-- create a @PQuery@ by using @parsePQuery@ in module Parser.
mkPQuery :: [Atom] -> PQuery
mkPQuery as
  = execState (forM_ ((\(Atom _ ts) -> ts) <$> as) worker) (PQuery S.empty as)
  where
    worker ts = forM_ ts $ \case
      VariableTerm v -> pqVariables %= S.insert v
      F _ ts'        -> worker ts'

-- | Retrieve the set of variables from a "FunctionTerm".
getVariables :: FunctionTerm -> Set Text
getVariables (FTerm _ ts) = S.unions (worker <$> ts)
  where
    worker (VariableTerm v) = S.singleton v
    worker (F _ as)         = S.unions (worker <$> as)

-- | Check if the @Program@ specification is consistent.
--
-- It checks the following:
-- 1. All identifiers are non-empty and start with a lower-case letter or a
--   digit, and the rest are alphanumeric.
-- 2. All variables are non-empty and start with an upper-case letter, and the
--  rest are alphanumeric.
-- 3. All predicates have the correct arity.
-- 4. All constants are in the set of constants as described by the @Program@.
-- 5. All variables are in the set of variables as described by the @Program@.
isProgramLegal :: Program -> Bool
isProgramLegal Program {..}
    -- Check legality of identifiers.
  = all indentifierLegal ( S.union (S.map _functionName _functions)
                                   (S.map _predicateName _predicates) )
    -- Check legality of variables.
 && all variableLegal _variables
    -- Check legality of clauses, which means checking all its components.
 && all clauseLegal _clauses
  where
    indentifierLegal name   = not (T.null name)
                           && (isLower (T.head name) || isDigit (T.head name))
                           && T.all isAlphaNum (T.tail name)
    variableLegal var       = not (T.null var) && isUpper (T.head var)
                           && T.all isAlphaNum (T.tail var)
    -- A term is legal if it is a constant and is in the set of constants, or
    -- it is a variable and is in the set of variables
    termLegal  term         = case term of
      VariableTerm v -> v `elem` _variables
      F f ts         -> f `elem` _functions && length ts == _functionArity f
                     && all termLegal ts
    -- An atom is legal if its predicate is in the set of predicates, has the
    -- correct arity, all its arguments are legal, and all its variables are
    -- legal.
    atomLegal (Atom p as)   =  _predicateArity p == length as
                            && p `elem` _predicates
                            && all termLegal as
    -- A clause is legal if its head and all its body atoms are legal.
    clauseLegal Clause {..} = maybe True atomLegal _clauseHead
                           && all atomLegal _clauseBody
