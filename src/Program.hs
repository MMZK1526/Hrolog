-- | Rexport the exposed API for Internal.Program, including the data types
-- @Program@ and @PQuery@, and the constructors for them.
module Program (
  Program, PQuery, emptyProgram, mkProgram, mkPQuery, isProgramLegal
  ) where

import           Internal.Program
