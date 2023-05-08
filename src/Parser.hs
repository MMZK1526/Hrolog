-- | Parsers for Hrolog programs and queries. It re-exports the exposed API of
-- the "Internal.Parser" module.
module Parser (
  parseProgram,
  parsePQuery,
  program,
  pQuery,
  pQuery'
) where

import           Internal.Parser
