module Utility where

import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Program

unifyTerm :: Term -> Term -> Maybe (Map String Term)
unifyTerm (VariableTerm x) t'@(ConstantTerm _) = Just (M.singleton x t') 
unifyTerm t t'@(VariableTerm _)                = unifyTerm t' t
unifyTerm (ConstantTerm c) (ConstantTerm c')   = M.empty <$ guard (c == c')

unifyAtom :: Atom -> Atom -> Maybe (Map String Term)
unifyAtom (Atom p ts) (Atom p' ts')
  | p /= p'   = Nothing
  | otherwise = foldl addToMap (Just M.empty) (zip ts ts')
  where
    addToMap mm (t, t') = do
      m <- mm
      unifyTerm (substituteTerm m t) (substituteTerm m t' )

substituteTerm :: Map String Term -> Term -> Term
substituteTerm m t@(VariableTerm x) = fromMaybe t (M.lookup x m)
substituteTerm _ t                  = t

substituteAtom :: Map String Term -> Atom -> Atom
substituteAtom m (Atom p ts) = Atom p (map (substituteTerm m) ts)
