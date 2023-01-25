{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Program where

import           Data.Char
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as S

data Predicate = Predicate { _predicateName :: String, _predicateArity :: Int }
  deriving (Eq, Ord)

newtype Constant = Constant { _constantName :: String }
  deriving (Eq, Ord)

data Term = ConstantTerm Constant
          | VariableTerm String
  deriving (Eq, Ord)

data Atom = Atom Predicate [Term]
  deriving (Eq, Ord)

data Clause = Clause { _clauseHead :: Maybe Atom, _clauseBody :: [Atom] }
  deriving (Eq, Ord)

pattern EmptyClause :: Clause
pattern EmptyClause <- emptyClause

pattern (:<-) :: Atom -> [Atom] -> Clause
pattern h :<- b <- Clause (Just h) b

pattern (:?<-) :: Maybe Atom -> [Atom] -> Clause
pattern mh :?<- b <- Clause mh b

pattern Constraint :: [Atom] -> Clause
pattern Constraint b <- Clause Nothing b

pattern Fact :: Atom -> Clause
pattern Fact h <- Clause (Just h) []

data Program = Program { _predicates :: Set Predicate
                       , _constants  :: Set Constant
                       , _clauses    :: [Clause] }
  deriving (Eq, Ord)

instance Show Predicate where
  show :: Predicate -> String
  show Predicate {..} = concat [_predicateName, "/", show _predicateArity]

instance Show Constant where
  show :: Constant -> String
  show = _constantName

instance Show Term where
  show :: Term -> String
  show (ConstantTerm c) = show c
  show (VariableTerm v) = v

instance Show Atom where
  show :: Atom -> String
  show (Atom p as)
    = concat [_predicateName p, "(", intercalate ", " (show <$> as), ")"]

instance Show Clause where
  show :: Clause -> String
  show Clause {..} = concat [maybe "" show _clauseHead, bodyStr, "."]
    where
      bodyStr = case _clauseBody of
        [] -> ""
        cs -> concat [ maybe "" (const " ") _clauseHead, "<- "
                     , intercalate ", " (show <$> cs) ]

instance Show Program where
  show :: Program -> String
  show Program {..}
    = intercalate "\n" [unlines (show <$> _clauses), csStr ++ psStr]
    where
      csStr = case S.toList _constants of
        [] -> ""
        cs -> concat ["Constants: ", intercalate ", " (show <$> cs), ".\n"]
      psStr = case S.toList _predicates of
        [] -> ""
        ps -> concat ["Predicates: ", intercalate ", " (show <$> ps), ".\n"]

emptyClause :: Clause
emptyClause = Clause Nothing []

emptyProgram :: Program
emptyProgram = Program S.empty S.empty []

isProgramLegal :: Program -> Bool
isProgramLegal Program {..}
  = all indentifierLegal ( S.union (S.map _constantName _constants)
                                   (S.map _predicateName _predicates) )
 && all clauseLegal _clauses
  where
    indentifierLegal name   = not (null name) && isLower (head name)
                           && all isAlphaNum (tail name)
    termLegal term          = case term of
      ConstantTerm c -> c `elem` _constants
      VariableTerm v -> not (null v) && isUpper (head v)
                     && all isAlphaNum (tail v)
    atomLegal (Atom p as)   =  _predicateArity p == length as
                           && p `elem` _predicates
                           && all termLegal as
    clauseLegal Clause {..} = maybe True atomLegal _clauseHead
                      && all atomLegal _clauseBody
