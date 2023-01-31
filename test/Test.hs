{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Char
import           Data.Functor
import           Parser
import           Program
import           System.Random
import           Test.HUnit

main :: IO ()
main = runTestTTAndExit
     $ TestList [ canParseVariable
                , genVarsAreValid
                , canParseConstant
                , genConstsAreValid
                , canParseTerm
                , genTermsAreValid
                , canParseAtom
                , genAtomsAreValid
                , canParseClause
                , genClausesAreValid ]

canParseVariable :: Test
canParseVariable
  = TestLabel "Can parse variable" . TestCase $ do
      assertValid "Empty string" Nothing (parse variable "")
      assertValid "Uppercase letter" (Just "A") (parse variable "A")
      assertValid "Uppercase word" (Just "Abc") (parse variable "Abc")
      assertValid "Lowercase" Nothing (parse variable "abc")
      assertValid "Underscore" Nothing (parse variable "_")

genVarsAreValid :: Test
genVarsAreValid
  = TestLabel "Generated variables are valid" . TestCase
  . forM_ [1..7] $ \l -> forM_ [0..114] $ \g -> do
      let v = fst $ genVariable l (mkStdGen g)
      assertValid ("Valid variable " ++ pShow v) (Just v) (parse variable v)

canParseConstant :: Test
canParseConstant
  = TestLabel "Can parse variable" . TestCase $ do
      let parseConstant str = evalState (parseT constant str) emptyProgram
      assertValid "Empty string" Nothing (parseConstant "")
      assertValid "Lowercase letter" (Just $ Constant "a") (parseConstant "a")
      assertValid "Lowercase word" (Just $ Constant "aBC") (parseConstant "aBC")
      assertValid "Uppercase" Nothing (parseConstant "ABC")
      assertValid "Underscore" Nothing (parseConstant "_")

genConstsAreValid :: Test
genConstsAreValid
  = TestLabel "Generated constants are valid" . TestCase
  . forM_ [1..7] $ \l -> forM_ [0..114] $ \g -> do
      let c                 = fst $ genConstant l (mkStdGen g)
      let parseConstant str = evalState (parseT constant str) emptyProgram
      assertValid ("Valid constant " ++ pShow c)
                  (Just c) (parseConstant (pShow c))

canParseTerm :: Test
canParseTerm
  = TestLabel "Can parse term" . TestCase $ do
      let parseTerm str = evalState (parseT term str) emptyProgram
      assertValid "Empty string" Nothing (parseTerm "")
      assertValid "Lowercase letter"
                  (Just . ConstantTerm $ Constant "a") (parseTerm "a")
      assertValid "Lowercase word"
                  (Just . ConstantTerm $ Constant "abc") (parseTerm "abc")
      assertValid "Uppercase" (Just $ VariableTerm "ABC") (parseTerm "ABC")
      assertValid "Underscore" Nothing (parseTerm "_")

genTermsAreValid :: Test
genTermsAreValid
  = TestLabel "Generated terms are valid" . TestCase
  . forM_ [1..7] $ \l -> forM_ [0..114] $ \g -> do
      let t             = fst $ genTerm l (mkStdGen g)
      let parseTerm str = evalState (parseT term str) emptyProgram
      assertValid ("Valid terms " ++ pShow t) (Just t) (parseTerm (pShow t))

canParseAtom :: Test
canParseAtom
  = TestLabel "Can parse atom" . TestCase $ do
      let parseAtom str = evalState (parseT atom str) emptyProgram
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
                                [ ConstantTerm $ Constant "qeii"
                                , ConstantTerm $ Constant "kciii" ] )
                  (parseAtom "mother(qeii, kciii)")
      assertValid "Missing parenthesis" Nothing (parseAtom "foo(")
      assertValid "Bad separator" Nothing (parseAtom "foo(x; y)")
      assertValid "Capital predicate name" Nothing (parseAtom "Foo(x; y)")

genAtomsAreValid :: Test
genAtomsAreValid
  = TestLabel "Generated atoms are valid" . TestCase
  . forM_ [1..3] $ \a -> forM_ [1..3] $ \l -> forM_ [0..114] $ \g -> do
      let t             = fst $ genAtom a l (mkStdGen g)
      let parseAtom str = evalState (parseT atom str) emptyProgram
      assertValid ("Valid atoms " ++ pShow t) (Just t) (parseAtom (pShow t))

canParseClause :: Test
canParseClause
  = TestLabel "Can parse clause" . TestCase $ do
      let parseClause str = evalState (parseT clause str) emptyProgram
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
      let parseClause str = evalState (parseT clause str) emptyProgram
      assertValid ("Valid atoms " ++ pShow c) (Just c) (parseClause (pShow c))


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

assertValid :: Eq a => PP a => Show a
            => String -> Maybe a -> Either e a -> Assertion
assertValid str Nothing act = case act of
    Left _  -> pure ()
    Right _ -> assertFailure str
assertValid str (Just a) act = case act of
    Left _   -> assertFailure str
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

-- | Generate a @Constant@ with the given length.
genConstant :: Int -> StdGen -> (Constant, StdGen)
genConstant = (first Constant .) . runState . worker 1 36
  where
    worker _ _ 0 = pure ""
    worker m n l = do
      r <- randomRS m n
      if | r == 0    -> pure ""
         | r <= 26   -> (chr (ord 'a' + r - 1) :) <$> worker 0 62 (l - 1)
         | r <= 36   -> (chr (ord '0' + r - 27) :) <$> worker 0 62 (l - 1)
         | otherwise -> (chr (ord 'A' + r - 37) :) <$> worker 0 62 (l - 1)

-- | Generate a @Variable@ with the given length.
genTerm :: Int -> StdGen -> (Term, StdGen)
genTerm l gen
  | r == 0    = first ConstantTerm $ genConstant l gen'
  | otherwise = first VariableTerm $ genVariable l gen'
  where
    (r, gen') = randomR @Int (0, 1) gen

-- | Generate an @Atom@ with the given arity and the maximum length in the names
-- for the predicate and arguments.
genAtom :: Int -> Int -> StdGen -> (Atom, StdGen)
genAtom arity len = runState worker
  where
    worker = do
      len'                  <- randomRS 1 (max 1 len)
      (Constant pName, gen) <- genConstant len' <$> get
      put gen
      ts                    <- replicateM arity $ do
        len''     <- randomRS 1 (max 1 len)
        (t, gen') <- genTerm len'' <$> get
        put gen'
        return t
      return $ Atom (Predicate pName arity) ts

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
        return $ Just at
      body   <- replicateM len $ do
        arity'    <- randomRS 0 arity
        (at, gen) <- genAtom arity' len <$> get
        put gen
        return at
      return $ Clause mHead body

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
      return c

randomRS :: Random a => Monad m => RandomGen s => a -> a -> StateT s m a
randomRS a b = do
  (n, gen') <- randomR (a, b) <$> get
  put gen' $> n
