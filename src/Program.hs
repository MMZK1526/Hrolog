{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Program where

import           Data.Char
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as S

class PP a where
  pShow :: a -> String

  pPrint :: a -> IO ()
  pPrint = putStrLn . pShow
  {-# INLINE pPrint #-}

  pPrint' :: a -> IO ()
  pPrint' = putStr . pShow
  {-# INLINE pPrint' #-}

instance PP String where
  pShow :: String -> String
  pShow = show

data Predicate = Predicate { _predicateName :: String, _predicateArity :: Int }
  deriving (Eq, Ord, Show)

newtype Constant = Constant { _constantName :: String }
  deriving (Eq, Ord, Show)

data Term = ConstantTerm Constant
          | VariableTerm String
  deriving (Eq, Ord, Show)

data Atom = Atom Predicate [Term]
  deriving (Eq, Ord, Show)

data Clause = Clause { _clauseHead :: Maybe Atom, _clauseBody :: [Atom] }
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

instance PP Predicate where
  pShow :: Predicate -> String
  pShow Predicate {..} = concat [_predicateName, "/", show _predicateArity]

instance PP Constant where
  pShow :: Constant -> String
  pShow = _constantName

instance PP Term where
  pShow :: Term -> String
  pShow (ConstantTerm c) = pShow c
  pShow (VariableTerm v) = v

instance PP Atom where
  pShow :: Atom -> String
  pShow (Atom p as)
    = concat [_predicateName p, "(", intercalate ", " (pShow <$> as), ")"]

instance PP Clause where
  pShow :: Clause -> String
  pShow Clause {..} = concat [maybe "" pShow _clauseHead, bodyStr, "."]
    where
      bodyStr = case _clauseBody of
        [] -> ""
        cs -> concat [ maybe "" (const " ") _clauseHead, "<- "
                     , intercalate ", " (pShow <$> cs) ]

instance PP Program where
  pShow :: Program -> String
  pShow Program {..}
    = intercalate "\n" [unlines (pShow <$> _clauses), csStr ++ psStr]
    where
      csStr = case S.toList _constants of
        [] -> ""
        cs -> concat ["Constants: ", intercalate ", " (pShow <$> cs), ".\n"]
      psStr = case S.toList _predicates of
        [] -> ""
        ps -> concat ["Predicates: ", intercalate ", " (pShow <$> ps), ".\n"]

emptyClause :: Clause
emptyClause = Clause Nothing []

emptyProgram :: Program
emptyProgram = Program S.empty S.empty []

mkProgram :: [Clause] -> Program
mkProgram = Program S.empty S.empty

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
