{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Char
import           Data.Functor
import           Data.Functor.Identity
import           GHC.Stack
import           System.Random
import           Test.HUnit

import           Internal.Parser
import           Internal.Program
import           Utility.PP

main :: IO ()
main = runTestTTAndExit
     $ TestList [ canParseVariable
                , genVarsAreValid
                , canParseFunction
                , genFunctionsAreValid
                , canParseTerm
                , genTermsAreValid
                , canParseAtom
                , genAtomsAreValid
                , canParseClause
                , genClausesAreValid
                , genProgramsAreValid
                , canParsePQuery
                , genPQueriesAreValid ]

canParseVariable :: Test
canParseVariable
  = TestLabel "Can parse variable" . TestCase $ do
      let parseVariable str = evalState (parseT space variable str) (Identity emptyProgram)
      assertValid "Empty string" Nothing (parseVariable "")
      assertValid "Uppercase letter" (Just "A") (parseVariable "A")
      assertValid "Uppercase word" (Just "Abc") (parseVariable "Abc")
      assertValid "Lowercase" Nothing (parseVariable "abc")
      assertValid "Underscore" Nothing (parseVariable "_")

genVarsAreValid :: Test
genVarsAreValid
  = TestLabel "Generated variables are valid" . TestCase
  . forM_ [1..7] $ \l -> forM_ [0..114] $ \g -> do
      let v                 = fst $ genVariable l (mkStdGen g)
      let parseVariable str = evalState (parseT space variable str) (Identity emptyProgram)
      assertValid ("Valid variable " ++ show v) (Just v) (parseVariable v)

canParseFunction :: Test
canParseFunction
  = TestLabel "Can parse function" . TestCase $ do
      let parseFunction str = evalState (parseT space functionTerm str) (Identity emptyProgram)
      assertValid "Empty string" Nothing (parseFunction "")
      assertValid "Lowercase letter" (Just $ FTerm (Function "a" 0) []) (parseFunction "a")
      assertValid "Lowercase word" (Just $ FTerm (Function "aBC" 0) []) (parseFunction "aBC")
      assertValid "Uppercase" Nothing (parseFunction "ABC")
      assertValid "Underscore" Nothing (parseFunction "_")

genFunctionsAreValid :: Test
genFunctionsAreValid
  = TestLabel "Generated functions are valid" . TestCase
  . forM_ [1..4] $ \d -> forM_ [1..4] $ \l -> forM_ [0..114] $ \g -> do
      let f                 = fst $ genFunction d l (mkStdGen g)
      let parseFunction str = evalState (parseT space functionTerm str) (Identity emptyProgram)
      assertValid ("Valid function " ++ show f) (Just f) (parseFunction (pShow f))

canParseTerm :: Test
canParseTerm
  = TestLabel "Can parse term" . TestCase $ do
      let parseTerm str = evalState (parseT space term str) (Identity emptyProgram)
      assertValid "Empty string" Nothing (parseTerm "")
      assertValid "Lowercase letter"
                  (Just $ Constant "a") (parseTerm "a")
      assertValid "Lowercase word"
                  (Just $ Constant "abc") (parseTerm "abc")
      assertValid "Uppercase" (Just $ VariableTerm "ABC") (parseTerm "ABC")
      assertValid "Underscore" Nothing (parseTerm "_")

genTermsAreValid :: Test
genTermsAreValid
  = TestLabel "Generated terms are valid" . TestCase
  . forM_ [1..4] $ \d -> forM_ [1..4] $ \l -> forM_ [0..114] $ \g -> do
      let t             = fst $ genTerm d l (mkStdGen g)
      let parseTerm str = evalState (parseT space term str) (Identity emptyProgram)
      assertValid ("Valid term " ++ show t) (Just t) (parseTerm (pShow t))

canParseAtom :: Test
canParseAtom
  = TestLabel "Can parse atom" . TestCase $ do
      let parseAtom str = evalState (parseT space atom str) (Identity emptyProgram)
      assertValid "Empty string" Nothing (parseAtom "")
      assertValid "Constant predicate"
                  (Just $ Atom (Predicate "allGood" 0) []) (parseAtom "allGood")
      assertValid "0-ary predicate"
                  (Just $ Atom (Predicate "foo" 0) []) (parseAtom "foo ( )")
      assertValid "Unary predicate"
                  (Just $ Atom (Predicate "flies" 1) [VariableTerm "X"])
                  (parseAtom "flies(X)")
      assertValid "Binary predicate"
                  ( Just $ Atom (Predicate "mother" 2)
                                [ Constant "qeii", Constant "kciii" ] )
                  (parseAtom "mother(qeii, kciii)")
      assertValid "Missing parenthesis" Nothing (parseAtom "foo(")
      assertValid "Bad separator" Nothing (parseAtom "foo(x; y)")
      assertValid "Capital predicate name" Nothing (parseAtom "Foo(x; y)")

genAtomsAreValid :: Test
genAtomsAreValid
  = TestLabel "Generated atoms are valid" . TestCase
  . forM_ [1..3] $ \a -> forM_ [1..3] $ \l -> forM_ [0..114] $ \g -> do
      let t             = fst $ genAtom a l (mkStdGen g)
      let parseAtom str = evalState (parseT space atom str) (Identity emptyProgram)
      assertValid ("Valid atom " ++ show t) (Just t) (parseAtom (pShow t))

canParseClause :: Test
canParseClause
  = TestLabel "Can parse clause" . TestCase $ do
      let parseClause str = evalState (parseT space clause str) (Identity emptyProgram)
      assertValid "Empty string" Nothing (parseClause "")
      assertValid "Empty clause" (Just $ Clause Nothing []) (parseClause ".")
      assertValid "Fact"
                  (Just $ Clause (Just $ Atom (Predicate "allGood" 0) []) [])
                  (parseClause "allGood.")
      assertValid "Constraint"
                  (Just $ Clause Nothing [Atom (Predicate "foo" 0) []])
                  (parseClause "  <-foo ( ).")
      assertValid "Definite Clause"
                  ( Just $ Clause ( Just $ Atom (Predicate "parent" 2)
                                                [ VariableTerm "X"
                                                , VariableTerm "Y" ] )
                           [ Atom (Predicate "father" 2)
                                  [ VariableTerm "X", VariableTerm "Y" ] ] )
                  (parseClause "parent(X, Y) <- father(X, Y).")
      assertValid "Missing period" Nothing (parseClause "bad(A, B)")

genClausesAreValid :: Test
genClausesAreValid
  = TestLabel "Generated clauses are valid" . TestCase
  . forM_ [1..3] $ \a -> forM_ [1..3] $ \l -> forM_ [0..114] $ \g -> do
      let c               = fst $ genClause a l (mkStdGen g)
      let parseClause str = evalState (parseT space clause str) (Identity emptyProgram)
      assertValid ("Valid clause: " ++ pShow c) (Just c) (parseClause (pShow c))

genProgramsAreValid :: Test
genProgramsAreValid
  = TestLabel "Generated programs are valid" . TestCase
  . forM_ [1..3] $ \a -> forM_ [1..3] $ \l -> forM_ [0..114] $ \g -> do
      let p = fst $ genProgram a l (mkStdGen g)
      assertBool ("Legal program: " ++ show p) (isProgramLegal p)
      assertValid ("Valid program " ++ show p)
                  (Just p) (parseProgram (pShowF Succinct p))

canParsePQuery :: Test
canParsePQuery
  = TestLabel "Can parse Prolog query" . TestCase $ do
      assertValid "Empty string" Nothing (parsePQuery Nothing "")
      assertValid "Empty query" (Just $ mkPQuery []) (parsePQuery Nothing ".")
      assertValid "Propositional atom"
                  ( Just $ mkPQuery [Atom (Predicate "me" 0) []] )
                  (parsePQuery Nothing "me.")
      assertValid "Singleton query"
                  ( Just $ mkPQuery [ Atom (Predicate "gt" 1)
                                           [Constant "a"] ] )
                  (parsePQuery Nothing "gt(a).")
      assertValid "Long query"
                  ( Just $ mkPQuery [ Atom (Predicate "gt" 1)
                                           [Constant "a"]
                                    , Atom (Predicate "foo" 2)
                                           [ VariableTerm "B"
                                           , Constant "c" ] ] )
                  (parsePQuery Nothing "gt(a), foo(B, c).")
      assertValid "Missing period" Nothing (parsePQuery Nothing "gt(a)")
      assertValid "Missing parenthesis" Nothing (parsePQuery Nothing "gt(a")
      assertValid "Capital letter predicate" Nothing (parsePQuery Nothing "Bad(apple)")

genPQueriesAreValid :: Test
genPQueriesAreValid
  = TestLabel "Generated prolog queries are valid" . TestCase
  . forM_ [1..3] $ \k -> forM_ [1..3] $ \l -> forM_ [0..114] $ \g -> do
      let c = fst $ genPQuery k l (mkStdGen g)
      assertValid ("Valid prolog query " ++ show c)
                  (Just c) (parsePQuery Nothing (pShow c))


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

assertValid :: HasCallStack => Eq a => Show a => String -> Maybe a -> Either e a -> Assertion
assertValid str Nothing act = case act of
    Left _  -> pure ()
    Right _ -> assertFailure $ "Shouldn't parse: " ++ str
assertValid str (Just a) act = case act of
    Left _   -> assertFailure $ "Failed to parse: " ++ str
    Right a' -> assertEqual str a a'

-- | Generate a variable with the given length.
genVariable :: Int -> StdGen -> (String, StdGen)
genVariable = runState . worker 1 26
  where
    worker _ _ 0 = pure ""
    worker m n l = do
      r <- randomRS m n
      if | r == 0    -> pure ""
         | r <= 26   -> (chr (ord 'A' + r - 1) :) <$> worker 0 62 (l - 1)
         | r <= 36   -> (chr (ord '0' + r - 27) :) <$> worker 0 62 (l - 1)
         | otherwise -> (chr (ord 'a' + r - 37) :) <$> worker 0 62 (l - 1)

-- | Generate an identifer with the given length.
genIdentifier :: Int -> StdGen -> (String, StdGen)
genIdentifier = runState . worker 1 26
  where
    worker _ _ 0 = pure ""
    worker m n l = do
      r <- randomRS m n
      if | r == 0    -> pure ""
         | r <= 26   -> (chr (ord 'a' + r - 1) :) <$> worker 0 62 (l - 1)
         | r <= 36   -> (chr (ord '0' + r - 27) :) <$> worker 0 62 (l - 1)
         | otherwise -> (chr (ord 'A' + r - 37) :) <$> worker 0 62 (l - 1)

-- | Generate a @FunctionTerm@ with the given length.
genFunction :: Int -> Int -> StdGen -> (FunctionTerm, StdGen)
genFunction depth len = runState (worker depth len)
  where
    worker 0 l = do
      len'        <- randomRS 1 (max 1 l)
      (name, gen) <- genIdentifier len' <$> get
      put gen
      pure $ FTerm (Function name 0) []
    worker d l = do
      len'        <- randomRS 1 (max 1 l)
      (name, gen) <- genIdentifier len' <$> get
      put gen
      arity       <- randomRS 1 (max 1 l)
      ts          <- replicateM arity $ do
        (t, gen') <- genTerm (d - 1) l <$> get
        put gen'
        pure t
      pure $ FTerm (Function name arity) ts

-- | Generate a @Variable@ with the given length.
genTerm :: Int -> Int -> StdGen -> (Term, StdGen)
genTerm d l gen
  | r == 0    = first FunctionTerm $ genFunction d l gen'
  | otherwise = first VariableTerm $ genVariable l gen'
  where
    (r, gen') = randomR @Int (0, 1) gen

-- | Generate an @Atom@ with the given arity and the maximum length in the names
-- for the predicate and arguments.
genAtom :: Int -> Int -> StdGen -> (Atom, StdGen)
genAtom arity len = runState worker
  where
    worker = do
      len'         <- randomRS 1 (max 1 len)
      (pName, gen) <- genIdentifier len' <$> get
      put gen
      ts           <- replicateM arity $ do
        depth     <- randomRS 1 (max 1 len)
        len''     <- randomRS 1 (max 1 len)
        (t, gen') <- genTerm depth len'' <$> get
        put gen'
        pure t
      pure $ Atom (Predicate pName arity) ts

-- | Generate a @Clause@ with the given body length, and the arity and length
-- of each atoms are at most "arity" and "len".
genClause :: Int -> Int -> StdGen -> (Clause, StdGen)
genClause arity len = runState worker
  where
    worker = do
      noHead <- even <$> randomRS @Int 0 1
      mHead  <- if noHead then pure Nothing else do
        arity'    <- randomRS 0 arity
        (at, gen) <- genAtom arity' len <$> get
        put gen
        pure $ Just at
      body   <- replicateM len $ do
        arity'    <- randomRS 0 arity
        (at, gen) <- genAtom arity' len <$> get
        put gen
        pure at
      pure $ Clause mHead body

-- | Generate a @Clause@ with the given number of clauses, and all other
-- parameters are upper-bounded by "maxParam".
genProgram :: Int -> Int -> StdGen -> (Program, StdGen)
genProgram lineCount maxParam = runState worker
  where
    worker = fmap mkProgram <$> replicateM lineCount $ do
      arity    <- randomRS 0 maxParam
      len      <- randomRS 0 maxParam
      (c, gen) <- genClause arity len <$> get
      put gen
      pure c

-- | Generate an @PQuery@ with @count@ @Atoms@a and all other parameters are
-- upper-bounded by "maxParam".
genPQuery :: Int -> Int -> StdGen -> (PQuery, StdGen)
genPQuery count maxParam = runState worker
  where
    worker = fmap mkPQuery . replicateM count $ do
      arity <- randomRS 0 maxParam
      (a, gen) <- genAtom arity maxParam <$> get
      put gen
      pure a

randomRS :: Random a => Monad m => RandomGen s => a -> a -> StateT s m a
randomRS a b = do
  (n, gen') <- randomR (a, b) <$> get
  put gen' $> n
