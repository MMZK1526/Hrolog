{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Program where

import           Data.Char
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as S

p1 :: Program
p1 = Program { _predicates = S.fromList [bird, flies, animal]
             , _constants  = S.fromList [penguin, plane, hawk, sparrow]
             , _clauses    = [ c1, c2, c3, c4, c5, c6, c7 ] }
  where
    bird    = Predicate "bird" 1
    flies   = Predicate "flies" 1
    animal  = Predicate "animal" 1
    penguin = Constant "penguin"
    plane   = Constant "plane"
    hawk    = Constant "hawk"
    sparrow = Constant "sparrow"
    x       = VariableAtom "X"
    c1      = Clause (Just $ PredicateAtom bird [ConstantAtom penguin]) []
    c2      = Clause (Just $ PredicateAtom bird [x])
                     [PredicateAtom flies [x], PredicateAtom animal [x]]
    c3      = Clause (Just $ PredicateAtom flies [ConstantAtom plane]) []
    c4      = Clause (Just $ PredicateAtom flies [ConstantAtom hawk]) []
    c5      = Clause (Just $ PredicateAtom flies [ConstantAtom sparrow]) []
    c6      = Clause (Just $ PredicateAtom animal [ConstantAtom hawk]) []
    c7      = Clause (Just $ PredicateAtom animal [ConstantAtom sparrow]) []

data Predicate = Predicate { _predicateName :: String, _predicateArity :: Int }
  deriving (Eq, Ord)

newtype Constant = Constant { _constantName :: String }
  deriving (Eq, Ord)

data Atom = ConstantAtom  Constant
          | VariableAtom  String
          | PredicateAtom Predicate [Atom]
  deriving (Eq, Ord)

data Clause = Clause { _clauseHead :: Maybe Atom, _clauseBody :: [Atom] }
  deriving (Eq, Ord)

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

instance Show Atom where
  show :: Atom -> String
  show (ConstantAtom c) = show c
  show (VariableAtom v) = v
  show (PredicateAtom p as)
    = concat [_predicateName p, "(", intercalate ", " (show <$> as), ")"]

instance Show Clause where
  show :: Clause -> String
  show Clause {..} = concat [maybe "" show _clauseHead, bodyStr, "."]
    where
      bodyStr = case _clauseBody of
        [] -> ""
        cs -> " <- " ++ intercalate ", " (show <$> cs)

instance Show Program where
  show :: Program -> String
  show Program {..} = unlines (show <$> _clauses)

isProgramLegal :: Program -> Bool
isProgramLegal Program {..}
  = all indentifierLegal ( S.union (S.map _constantName _constants)
                                   (S.map _predicateName _predicates) )
 && all clauseLegal _clauses
  where
    indentifierLegal name   = not (null name) && isLower (head name)
                           && all isAlphaNum (tail name)
    atomLegal a             = case a of
      ConstantAtom c     -> c `elem` _constants
      VariableAtom v     -> not (null v) && isUpper (head v)
                         && all isAlphaNum (tail v)
      PredicateAtom p as -> _predicateArity p == length as
                         && p `elem` _predicates
                         && all atomLegal as
    clauseLegal Clause {..} = maybe True atomLegal _clauseHead
                      && all atomLegal _clauseBody
