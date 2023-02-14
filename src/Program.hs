{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Program where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Char
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as S

data PPOp = Verbose | Succinct

class PP p a where
  pShowF :: p -> a -> String

  pPrintF :: p -> a -> IO ()
  pPrintF = (putStrLn .) . pShowF
  {-# INLINE pPrintF #-}

  pPrintF' :: p -> a -> IO ()
  pPrintF' = (putStr .) . pShowF
  {-# INLINE pPrintF' #-}

pShow :: PP () a => a -> String
pShow = pShowF ()
{-# INLINE pShow #-}

pPrint :: PP () a => a -> IO ()
pPrint = pPrintF ()
{-# INLINE pPrint #-}

pPrint' :: PP () a => a -> IO ()
pPrint' = pPrintF' ()
{-# INLINE pPrint' #-}

instance PP () String where
  pShowF :: () -> String -> String
  pShowF = const show

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

newtype PQuery = PQuery [Atom]
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
  pShowF _ (Atom p as)
    = concat [_predicateName p, "(", intercalate ", " (pShow <$> as), ")"]

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
    Verbose  -> intercalate "\n" [clStr, csStr, psStr]
    where
      clStr = unlines (pShow <$> _clauses)
      csStr = case S.toList _constants of
        [] -> ""
        cs -> concat ["Constants: ", intercalate ", " (pShow <$> cs), ".\n"]
      psStr = case S.toList _predicates of
        [] -> ""
        ps -> concat ["Predicates: ", intercalate ", " (pShow <$> ps), ".\n"]

instance PP () PQuery where
  pShowF :: () -> PQuery -> String
  pShowF _ (PQuery as) = intercalate ", " (pShow <$> as) ++ "."

-- | The empty @Clause@ (failure).
emptyClause :: Clause
emptyClause = Clause Nothing []

-- | The empty @Program@ (entails nothing).
emptyProgram :: Program
emptyProgram = Program S.empty S.empty []

-- | Turn a list of @Clause@s into a @Program@ by calculating the set of
-- predicates and constants.
mkProgram :: [Clause] -> Program
mkProgram cs
  = uncurry Program (execState (forM_ cs workClause) (S.empty, S.empty)) cs
  where
    workClause (Clause mHead body) = do
      forM_ mHead workAtom
      forM_ body workAtom
    workAtom (Atom p ts)           = do
      modify (first (S.insert p))
      forM_ ts workTerm
    workTerm (ConstantTerm c)      = modify (second (S.insert c))
    workTerm _                     = pure ()

-- | Check if the @Program@ specification is consistent.
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
