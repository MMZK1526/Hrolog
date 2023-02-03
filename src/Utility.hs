module Utility where

import           Control.Applicative
import           Control.Monad
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Functor
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Program

data UnifyNode = UnifyNode { _unValue :: Maybe Constant
                           , _unVars  :: Set String }

data UnifyState = UnifyState { _usNode    :: Int
                             , _usVarMap  :: Map String Int
                             , _usNodeMap :: IntMap UnifyNode }

mkCUN :: Constant -> String -> UnifyNode
mkCUN c var = UnifyNode (Just c) (S.singleton var)

mkUN :: [String] -> UnifyNode
mkUN = UnifyNode Nothing . S.fromList

mergeUN :: UnifyNode -> UnifyNode -> Maybe UnifyNode
mergeUN un un' = do
  guard (fromMaybe True $ liftM2 (==) (_unValue un) (_unValue un'))
  return $ UnifyNode (_unValue un <|> _unValue un')
                     (S.union (_unVars un) (_unVars un'))

addUN :: String -> UnifyNode -> UnifyNode
addUN var un = un { _unVars = S.insert var (_unVars un) }

emptyUS :: UnifyState
emptyUS = UnifyState 0 M.empty IM.empty

unifyTerm :: Term -> Term -> Maybe (Maybe (String, Term))
unifyTerm (VariableTerm x) t'@(ConstantTerm _) = Just $ Just (x, t')
unifyTerm t t'@(VariableTerm _)                = unifyTerm t' t
unifyTerm (ConstantTerm c) (ConstantTerm c')   = Nothing <$ guard (c == c')

unifyAtom :: Atom -> Atom -> Maybe (Map String Term)
unifyAtom (Atom p ts) (Atom p' ts')
  | p /= p'   = Nothing
  | otherwise = case foldM worker emptyUS (zip ts ts') of
    Nothing -> Nothing
    Just us -> Just $ foldl' (reducer us) M.empty (M.assocs $ _usVarMap us) 
  where
    reducer us sub (var, vid) = case _usNodeMap us IM.!? vid of
      Nothing -> sub
      Just un -> case _unValue un of
        Just c  -> M.insert var (ConstantTerm c) sub
        Nothing -> case fst <$> S.minView (_unVars un) of
          Nothing -> sub
          Just v  -> M.insert var (VariableTerm v) sub
    worker us (t, t')         = do
      mUnified <- unifyTerm t t'
      unified  <- mUnified
      let n    = _usNode us
      let vMap = _usVarMap us
      let nMap = _usNodeMap us
      case unified of
        (var, ConstantTerm c) -> case vMap M.!? var of
          Nothing -> return us { _usNode    = n + 1
                               , _usVarMap  = M.insert var n vMap
                               , _usNodeMap = IM.insert n (mkCUN c var) nMap }
          Just n' -> guard (Just c == _unValue (nMap IM.! n')) $> us
        (var, VariableTerm v) -> case (vMap M.!? var, vMap M.!? v) of
          (Just n', Just n'') -> if n' == n''
            then pure us
            else do
              un' <- mergeUN (nMap IM.! n') (nMap IM.! n'')
              return us { _usNodeMap = IM.insert n' un'
                                     $ IM.insert n'' un' nMap }
          (Nothing, Nothing)  -> do
            return us { _usNode    = n + 1
                      , _usVarMap  = M.insert var n $ M.insert v n vMap
                      , _usNodeMap = IM.insert n (mkUN [var, v]) nMap }
          (Just n', Nothing)  -> do
            return us { _usVarMap  = M.insert v n' vMap
                      , _usNodeMap = IM.adjust (addUN v) n' nMap }
          (Nothing, Just n'') -> do
            return us { _usVarMap  = M.insert var n'' vMap
                      , _usNodeMap = IM.adjust (addUN var) n'' nMap }

substituteTerm :: Map String Term -> Term -> Term
substituteTerm m t@(VariableTerm x) = fromMaybe t (M.lookup x m)
substituteTerm _ t                  = t

substituteAtom :: Map String Term -> Atom -> Atom
substituteAtom m (Atom p ts) = Atom p (map (substituteTerm m) ts)
