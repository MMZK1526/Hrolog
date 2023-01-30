module Solver where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Program ( Program(Program), Term, Clause (Clause), Atom )
import qualified Program as P
import Utility (unifyAtom)

solve :: Program -> Atom -> [Map String Term]
solve = solve' M.empty

solve' :: Map String Term -> Program -> Atom -> [Map String Term]
-- solve' = undefined
solve' _ (Program _ _ cs) t =
  catMaybes
    ( do
        Clause {P._clauseHead = h, P._clauseBody = _} <- cs
        case h of
            Nothing -> return Nothing
            Just t' -> return (unifyAtom t t')
    )
