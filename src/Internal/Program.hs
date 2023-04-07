{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- See "test/programs/" for examples of Hrolog programs.
--
-- See "Program" for the exposed API.
module Internal.Program where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Char
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Utility.PP

-- | A Hrolog predicate with its name and arity.
data Predicate = Predicate { _predicateName :: String, _predicateArity :: Int }
  deriving (Eq, Ord, Show)

-- | A Hrolog constant, starting with a capital letter.
newtype Constant = Constant { _constantName :: String }
  deriving (Eq, Ord, Show)

-- | A Hrolog term, either a constant or a variable.
--
-- In a real program, the type variable "a" should always be a @String@, but it
-- can carry additional metadata during the solving process.
data Term' a = ConstantTerm Constant
             | VariableTerm a
  deriving (Eq, Ord, Show)
type Term = Term' String

-- | A Hrolog atom, consisting of a predicate and a list of terms as arguments.
data Atom' a = Atom Predicate [Term' a]
  deriving (Eq, Ord, Show)
type Atom = Atom' String

-- | A Hrolog clause, consisting of an optional head and a list of bodies.
--
-- It represents @[H] <- B1, B2, ..., Bn@.
data Clause' a = Clause { _clauseHead :: Maybe (Atom' a)
                        , _clauseBody :: [Atom' a] }
  deriving (Eq, Ord, Show)
type Clause = Clause' String

-- | The pattern for the empty clause []. It is used to represent failure.
pattern EmptyClause :: Clause
pattern EmptyClause = Clause Nothing []

-- | The pattern for a clause with a head and a list of bodies.
pattern (:<-) :: Atom' a -> [Atom' a] -> Clause' a
pattern h :<- b <- Clause (Just h) b

-- | The pattern for a clause with an optional head and a list of bodies.
pattern (:?<-) :: Maybe (Atom' a) -> [Atom' a] -> Clause' a
pattern mH :?<- b <- Clause mH b

-- | The pattern for a constraint with no head.
pattern Constraint :: [Atom' a] -> Clause' a
pattern Constraint b <- Clause Nothing b

-- | The pattern for a fact with no body.
pattern Fact :: Atom' a -> Clause' a
pattern Fact h <- Clause (Just h) []

-- | The data type for a Hrolog program, consisting of a series of clauses.
-- It also contains the set of predicates, constants, and variables used in the
-- program.
data Program' a = Program { _predicates :: Set Predicate
                          , _constants  :: Set Constant
                          , _variables  :: Set String
                          , _clauses    :: [Clause' a] }
  deriving (Eq, Ord, Show)
type Program = Program' String

makeLenses ''Program'

-- | The data type for a Hrolog query, consisting the list of query atoms.
-- It also contains the set of variables used in the query.
--
-- It represents the query @<- Q1, Q2, ..., Qn@.
data PQuery' a = PQuery { _pqVariables :: Set a
                        , _pqAtoms     :: [Atom' a] }
  deriving (Eq, Ord, Show)
type PQuery = PQuery' String

makeLenses ''PQuery'

-- | The data type for a single Hrolog solution, consisting of a substitution
-- map from variables to terms.
newtype Solution = Solution (Map String Term)
  deriving (Eq, Ord, Show)

instance PP () Predicate where
  pShowF :: () -> Predicate -> String
  pShowF _ Predicate {..} = concat [_predicateName, "/", show _predicateArity]

instance PP () Constant where
  pShowF :: () -> Constant -> String
  pShowF = const _constantName

instance PP () Term where
  pShowF :: () -> Term -> String
  pShowF _ (ConstantTerm c) = pShow c
  pShowF _ (VariableTerm v) = v

instance PP () Atom where
  pShowF :: () -> Atom -> String
  pShowF _ (Atom p as) = case _predicateArity p of
    0 -> _predicateName p
    _ -> concat [_predicateName p, "(", intercalate ", " (pShow <$> as), ")"]

instance PP () Clause where
  pShowF :: () -> Clause -> String
  pShowF _ Clause {..} = concat [maybe "" pShow _clauseHead, bodyStr, "."]
    where
      bodyStr = case _clauseBody of
        [] -> ""
        cs -> concat [ maybe "" (const " ") _clauseHead, "<- "
                     , intercalate ", " (pShow <$> cs) ]

instance PP () Program where
  pShowF :: () -> Program -> String
  pShowF = const $ pShowF Verbose

instance PP PPOp Program where
  pShowF :: PPOp -> Program -> String
  pShowF vbLvl Program {..} = case vbLvl of
    Succinct -> clStr
    Verbose  -> intercalate "\n" [clStr, csStr, vsStr, psStr]
    where
      clStr = unlines (pShow <$> _clauses)
      csStr = case S.toList _constants of
        [] -> ""
        cs -> concat ["Constants: ", intercalate ", " (pShow <$> cs), ".\n"]
      vsStr = case S.toList _variables of
        [] -> ""
        vs -> concat ["Variables: ", intercalate ", " vs, ".\n"]
      psStr = case S.toList _predicates of
        [] -> ""
        ps -> concat ["Predicates: ", intercalate ", " (pShow <$> ps), ".\n"]

instance PP () PQuery where
  pShowF :: () -> PQuery -> String
  pShowF _ (PQuery _ as) = intercalate ", " (pShow <$> as) ++ "."

instance PP () Solution where
  pShowF :: () -> Solution -> String
  pShowF () (Solution sMap)
    | M.null sMap = "Valid\n"
    | otherwise   = concatMap showEntry (M.toAscList sMap)
    where
      showEntry (v, t) = concat [v, " = ", pShow t, ";\n"]

-- | The empty @Clause@ (failure).
emptyClause :: Clause
emptyClause = EmptyClause

-- | The empty @Program@ (entails nothing).
emptyProgram :: Program
emptyProgram = Program S.empty S.empty S.empty []

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
      modify (predicates %~ S.insert p)
      forM_ ts workTerm
    workTerm (ConstantTerm c)      = modify (constants %~ S.insert c)
    workTerm (VariableTerm v)      = modify (variables %~ S.insert v)

-- | Turn a list of @Atom@s into a @PQuery@ by calculating the set of variables.
--
-- This function is internal and is only used for testing. The user should
-- create a @PQuery@ by using @parsePQuery@ in module Parser.
mkPQuery :: [Atom] -> PQuery
mkPQuery as
  = execState (forM_ as worker) (PQuery S.empty as)
  where
    worker (Atom _ ts) = forM_ ts $ \case
      VariableTerm v -> modify (pqVariables %~ S.insert v)
      _              -> pure ()

-- | Check if the @Program@ specification is consistent.
isProgramLegal :: Program -> Bool
isProgramLegal Program {..}
  = all indentifierLegal ( S.union (S.map _constantName _constants)
                                   (S.map _predicateName _predicates) )
 && all variableLegal _variables
 && all clauseLegal _clauses
  where
    indentifierLegal name   = not (null name) && isLower (head name)
                           && all isAlphaNum (tail name)
    variableLegal var       = not (null var) && isUpper (head var)
                           && all isAlphaNum (tail var)
    termLegal term          = case term of
      ConstantTerm c -> c `elem` _constants
      VariableTerm v -> v `elem` _variables
    atomLegal (Atom p as)   =  _predicateArity p == length as
                           && p `elem` _predicates
                           && all termLegal as
    clauseLegal Clause {..} = maybe True atomLegal _clauseHead
                           && all atomLegal _clauseBody
