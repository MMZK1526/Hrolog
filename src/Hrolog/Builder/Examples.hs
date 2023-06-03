{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hrolog.Builder.Examples where

import           Hrolog.Builder

import           Program

-- | Define "src/Test/programs/peanoNumbers.hrolog" using the DSL.
--
-- >>> prettifyProgram peanoNumbers
-- "add(0, Y, Y).\nadd(s(X), Y, s(Z)) <- add(X, Y, Z).\nsub(X, Y, Z) <- add(Y, Z, X).\neq(X, X).\nne(X, Y) <- !eq(X, Y).\nfib(0, 0).\nfib(s(0), s(0)).\nfib(s(s(X)), Y) <- fib(X, A), fib(s(X), B), add(A, B, Y).\n"
peanoNumbers :: Program
-- We use the "program" combinator to define a program.
-- Note that all other combinators inside the "program" combinator has a "_"
-- suffix. This indicates that they produces a "Builder" value, which can be
-- used in a nested manner to build a program. At the top-level, we use the
-- non-"_" version of the combinators to run the "Builder" and construct the
-- actual ADT.
peanoNumbers = program do
  -- add(0, Y, Y).
  -- The "fact_" combinator constructs a fact (a clause without a body).
  -- Inside, the "atom_" combinator constructs an atom (a predicate with 0 or
  -- more arguments). The name of the atom is given as the first argument
  -- ("add"), and its argument are provided in a do-block.
  fact_ $ atom_ "add" do
    -- 0 (Constant).
    -- The "lit_" combinator is used to construct variables and constants.
    -- It takes a "Text" as its argument, which is the name of the variable or
    -- constant (depending on whether the first character is capitalised).
    -- It requires the "OverloadedStrings" extension to work.
    lit_ "0"
    -- Y (Variable).
    -- Here is an example where the first character is capitalised, so it is a
    -- variable.
    lit_ "Y"
    lit_ "Y"
  -- add(s(X), Y, s(Z)) <- add(X, Y, Z).
  -- The "<-|" combinator constructs a rule (a clause with a body).
  -- In this rule, we construct the arguments of the head atom "add" using
  -- semigroup append (<>), which is an alternative to using do-blocks.
  -- In this way, we can omit the "lit_" combinator for constants and variables.
  -- The "func_" combinator is used to construct function terms (an identifier
  -- and a list of arguments). It takes a "Text" as its first argument, which
  -- is the name of the function, and a list of "Builder" values as its second
  -- argument, which are the arguments of the function. It is very similar to
  -- the "atom_" combinator.
  atom_ "add" (func_ "s" "X" <> "Y" <> func_ "s" "Z")
    <-| atom_ "add" do
      -- X (Variable).
      -- If we remove the "lit_" combinator, we would need to specify the type
      -- for GHC to disambiguate the overloaded string literal. This is just
      -- a demonstration as I believe using "lit_" is more readable.
      _ :: () <- "X"
      lit_ "Y"
      lit_ "Z"
  -- sub(X, Y, Z) <- add(Y, Z, X).
  -- Here is the third way to construct arguments: using the list syntax.
  -- It requires the "OverloadedLists" extension to work.
  atom_ "sub" (lit_ "X" <> lit_ "Y" <> lit_ "Z")
    <-| atom_ "add" ["Y", "Z", "X"]
  -- eq(X, X).
  fact_ $ atom_ "eq" ["X", "X"]
  -- ne(X, Y) <- !eq(X, Y).
  -- The negation is constructed by prefixing "!" in the name literal of the
  -- predicate.
  atom_ "ne" ["X", "Y"] <-| atom_ "!eq" ["X", "Y"]
  -- fib(0, 0).
  fact_ $ atom_ "fib" do
    lit_ "0"
    lit_ "0"
  -- fib(s(0), s(0)).
  fact_ $ atom_ "fib" do
    func_ "s" (lit_ "0")
    func_ "s" (lit_ "0")
  -- fib(s(s(X)), Y) <- fib(X, A), fib(s(X), B), add(A, B, Y).
  -- This rule has multiple bodies, so we have a do-block immediately after the
  -- (<-|) combinator.
  -- You may wonder what would happen if we pass a do block with multiple (or
  -- even zero) atoms to the head. In that case it would not compile since
  -- a rule must have exactly one head atom.
  atom_ "fib" (func_ "s" (func_ "s" "X") <> "Y")
    <-| do
      atom_ "fib" ["X", "A"]
      atom_ "fib" [func_ "s" "X", "B"]
      atom_ "add" ["A", "B", "Y"]

-- | Define "src/Test/programs/facts.hrolog" using the DSL.
facts :: Program
facts = program do
  -- Note that an predicate with an arity of 0 can be simply written as a
  -- literal.
  fact_ "a"
  fact_ $ atom_ "b" ["X"]
  fact_ $ atom_ "c" ["A", "B"]

-- | Define "src/Test/programs/firstBranchTerminates.hrolog" using the DSL.
firstBranchTerminates :: Program
firstBranchTerminates = program do
  fact_ "a"
  "a" <-| "a"

-- | Define "src/Test/programs/secondBranchTerminates.hrolog" using the DSL.
simpleNumbers :: Program
simpleNumbers = program do
  fact_ $ atom_ "succ" ["1", "0"]
  fact_ $ atom_ "succ" ["2", "1"]
  fact_ $ atom_ "succ" ["3", "2"]
  atom_ "gt" ["X", "Y"] <-| atom_ "succ" ["X", "Y"]
  atom_ "gt" ["X", "Y"] <-| do
    atom_ "succ" ["X", "Z"]
    atom_ "gt" ["Z", "Y"]
