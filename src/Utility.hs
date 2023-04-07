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

import           Internal.Program

data UnifyNode = UnifyNode { unValue :: Maybe Constant
                           , unVars  :: Set String }

data UnifyState = UnifyState { usNode    :: Int
                             , usVarMap  :: Map String Int
                             , usNodeMap :: IntMap UnifyNode }

mkCUN :: Constant -> String -> UnifyNode
mkCUN c var = UnifyNode (Just c) (S.singleton var)
{-# INLINE mkCUN #-}

mkUN :: [String] -> UnifyNode
mkUN = UnifyNode Nothing . S.fromList
{-# INLINE mkUN #-}

mergeUN :: UnifyNode -> UnifyNode -> Maybe UnifyNode
mergeUN un un' = do
  guard (fromMaybe True $ liftM2 (==) (unValue un) (unValue un'))
  return $ UnifyNode (unValue un <|> unValue un')
                     (S.union (unVars un) (unVars un'))
{-# INLINE mergeUN #-}

addUN :: String -> UnifyNode -> UnifyNode
addUN var un = un { unVars = S.insert var (unVars un) }
{-# INLINE addUN #-}

emptyUS :: UnifyState
emptyUS = UnifyState 0 M.empty IM.empty
{-# INLINE emptyUS #-}

unifyTerm :: Term -> Term -> Maybe (Maybe (String, Term))
unifyTerm (VariableTerm x) t'                = Just $ Just (x, t')
unifyTerm t t'@(VariableTerm _)              = unifyTerm t' t
unifyTerm (ConstantTerm c) (ConstantTerm c') = Nothing <$ guard (c == c')

unifyAtom :: Atom -> Atom -> Maybe (Map String Term)
unifyAtom (Atom p ts) (Atom p' ts')
  | p /= p'   = Nothing
  | otherwise = case foldM worker emptyUS (zip ts ts') of
    Nothing -> Nothing
    Just us -> Just $ foldl' (reducer us) M.empty (M.assocs $ usVarMap us)
  where
    reducer us sub (var, vid) = case usNodeMap us IM.!? vid of
      Nothing -> sub
      Just un -> case unValue un of
        Just c  -> M.insert var (ConstantTerm c) sub
        Nothing -> case fst <$> S.minView (unVars un) of
          Nothing -> sub
          Just v  -> M.insert var (VariableTerm v) sub
    worker us (t, t')         = do
      mUnified <- unifyTerm t t'
      case mUnified of
        Nothing      -> pure us
        Just unified -> do
          let n    = usNode us
          let vMap = usVarMap us
          let nMap = usNodeMap us
          case unified of
            (var, ConstantTerm c) -> case vMap M.!? var of
              Nothing -> do
                return us { usNode    = n + 1
                          , usVarMap  = M.insert var n vMap
                          , usNodeMap = IM.insert n (mkCUN c var) nMap }
              Just n' -> guard (Just c == unValue (nMap IM.! n')) $> us
            (var, VariableTerm v) -> case (vMap M.!? var, vMap M.!? v) of
              (Just n', Just n'') -> if n' == n''
                then pure us
                else do
                  un' <- mergeUN (nMap IM.! n') (nMap IM.! n'')
                  return us { usNodeMap = IM.insert n' un'
                                         $ IM.insert n'' un' nMap }
              (Nothing, Nothing)  -> do
                return us { usNode    = n + 1
                          , usVarMap  = M.insert var n $ M.insert v n vMap
                          , usNodeMap = IM.insert n (mkUN [var, v]) nMap }
              (Just n', Nothing)  -> do
                return us { usVarMap  = M.insert v n' vMap
                          , usNodeMap = IM.adjust (addUN v) n' nMap }
              (Nothing, Just n'') -> do
                return us { usVarMap  = M.insert var n'' vMap
                          , usNodeMap = IM.adjust (addUN var) n'' nMap }

substituteTerm :: Map String Term -> Term -> Term
substituteTerm m t@(VariableTerm x) = fromMaybe t (M.lookup x m)
substituteTerm _ t                  = t
{-# INLINE substituteTerm #-}

substituteAtom :: Map String Term -> Atom -> Atom
substituteAtom m (Atom p ts) = Atom p (map (substituteTerm m) ts)
{-# INLINE substituteAtom #-}

renameTerm :: String -> Term -> Term
renameTerm pre (VariableTerm x) = VariableTerm $ pre ++ x
renameTerm _ t                  = t
{-# INLINE renameTerm #-}

renameAtom :: String -> Atom -> Atom
renameAtom pre (Atom p ts) = Atom p (map (renameTerm pre) ts)
{-# INLINE renameAtom #-}
