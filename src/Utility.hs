module Utility where

import           Data.Map (Map)
import           Program

unifyTerm :: Term -> Term -> Maybe (Map String Term)
unifyTerm = undefined

unifyAtom :: Atom -> Atom -> Maybe (Map String Term)
unifyAtom = undefined

substituteTerm :: Map String Term -> Term -> Term
substituteTerm = undefined

substituteAtom :: Map String Term -> Atom -> Atom
substituteAtom = undefined
