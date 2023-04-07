-- | Rexport the exposed API for Internal.Program, including the data types
-- @Program@ and @PQuery@.
module Program (
  Program, PQuery, Solution, emptyProgram, isProgramLegal, prettifyProgram
  , prettifySolution
  ) where

import           Internal.Program
