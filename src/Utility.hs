module Utility where

import           Data.Map (Map, singleton, empty, lookup)
import           Program
import Data.Maybe (fromMaybe)

unifyTerm :: Term -> Term -> Maybe (Map String Term)
unifyTerm (VariableTerm x) t'@(ConstantTerm _) = Just (singleton x t') 
unifyTerm t t'@(VariableTerm _) = unifyTerm t' t
unifyTerm (ConstantTerm c) (ConstantTerm c')
    | c == c'   = Just empty
    | otherwise = Nothing

unifyAtom :: Atom -> Atom -> Maybe (Map String Term)
unifyAtom (Atom p ts) (Atom p' ts')
    | p /= p'   = Nothing
    | otherwise = undefined

substituteTerm :: Map String Term -> Term -> Term
substituteTerm m t@(VariableTerm x) = fromMaybe t (Data.Map.lookup x m)
substituteTerm _ t                  = t


substituteAtom :: Map String Term -> Atom -> Atom
substituteAtom m (Atom p ts) = Atom p (map (substituteTerm m) ts)
