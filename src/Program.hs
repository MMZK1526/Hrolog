{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Program where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Char
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
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
pattern mH :?<- b <- Clause mH b

pattern Constraint :: [Atom] -> Clause
pattern Constraint b <- Clause Nothing b

pattern Fact :: Atom -> Clause
pattern Fact h <- Clause (Just h) []

data Program = Program { _predicates :: Set Predicate
                       , _constants  :: Set Constant
                       , _variables  :: Set String
                       , _clauses    :: [Clause] }
  deriving (Eq, Ord, Show)

data PQuery = PQuery { _pqVariables :: Set String
                     , _pqAtoms     :: [Atom] }
  deriving (Eq, Ord, Show)

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
emptyClause = Clause Nothing []

-- | The empty @Program@ (entails nothing).
emptyProgram :: Program
emptyProgram = Program S.empty S.empty S.empty []

-- | Turn a list of @Clause@s into a @Program@ by calculating the set of
-- predicates, constants, and variables.
mkProgram :: [Clause] -> Program
mkProgram cs
  = execState (forM_ cs workClause) (Program S.empty S.empty S.empty cs)
  where
    workClause (Clause mHead body) = do
      forM_ mHead workAtom
      forM_ body workAtom
    workAtom (Atom p ts)           = do
      prog@Program {..} <- get
      put $ prog { _predicates = S.insert p _predicates }
      forM_ ts workTerm
    workTerm (ConstantTerm c)      = do
      prog@Program {..} <- get
      put $ prog { _constants = S.insert c _constants }
    workTerm (VariableTerm v)      = do
      prog@Program {..} <- get
      put $ prog { _variables = S.insert v _variables }

-- | Turn a list of @Atom@s into a @PQuery@ by calculating the set of variables.
mkPQuery :: [Atom] -> PQuery
mkPQuery as
  = execState (forM_ as worker) (PQuery S.empty as)
  where
    worker (Atom _ ts) = forM_ ts $ \case
      VariableTerm v ->
        modify (\q -> q { _pqVariables = S.insert v (_pqVariables q) })
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
