{-# LANGUAGE TupleSections #-}
module Core.Utility.InstanceResolver where
  import Core.Inference.Type.AST (TypedAST(..), Type(..), Class(..), getType)
  import Core.Inference.Type.Pretty (show')
  import Control.Arrow (Arrow(second, first))
  import Debug.Trace (traceShow)
  import Core.Inference.Type.Methods (Types(tyUnify, tyApply), constraintCheck, tyCompose)
  import Data.Either (isRight, isLeft, fromRight)
  import Data.Set (Set, toList, fromList, empty, union, unions)
  import Core.Inference.Type (createTypeInstName, createInstName, appify, isInstanceName)
  import qualified Data.Map as M
  import Data.List (nub, find, intercalate)
  import Data.Bifunctor (Bifunctor(bimap))
  import Data.Maybe (fromMaybe)
  import Control.Monad.Except (MonadError (throwError))
  
  containsTVar :: Type -> Bool
  containsTVar (TVar _) = True
  containsTVar (TApp t1 t2) = containsTVar t1 || containsTVar t2
  containsTVar (t1 :-> t2) = all containsTVar t1 || containsTVar t2
  containsTVar _ = False

  buildFun :: TypedAST -> [TypedAST] -> TypedAST
  buildFun t [] = t
  buildFun t (x:xs) = AppE (buildFun t xs) [x] (getType x)

  extract :: Type -> [Type]
  extract (TApp n x) = n : extract x
  extract x = [x]

  fromApp :: Type -> Class
  fromApp (TApp (TId cls) tys) = IsIn cls $ extract tys
  fromApp _ = error "fromApp: not a class"

  addArgument :: MonadError String m => [([Class], (String, [Class]))] -> TypedAST -> m (TypedAST, Set (String, Type))
  addArgument e a@(AbsE args body) = do
    (args', tc) <- addArgument e body
    return (AbsE args args', tc)
  addArgument e (AppE e1 e2 t') = do
    (e1', tc1) <- addArgument e e1
    t <- unzip <$> mapM (addArgument e) e2
    return (AppE e1' (fst t) t', tc1 `union` unions (snd t))
  addArgument e (LetE n@(name, _) v) = do
    (b', tc) <- addArgument e v
    return (LetE n (if null tc then b' else AbsE (toList tc) b') , tc)
  addArgument env (LetInE n@(name, _) v e) = do
    (v', tc') <- addArgument env v
    (e', tc) <- addArgument env e
    return (LetInE n (if null tc' then v' else AbsE (toList tc') (addDictOnRecursive v' (name, tc'))) e', tc)
  addArgument env (InstE n t) = case t of
    cls :=> ty -> do
      (calls, tcs) <- find' cls env
      return (AppE (VarE n ty) (reverse calls) ty, fromList tcs)
    _ -> throwError $ n ++ " should have a class constraint"
  addArgument env (PatternE p e) = do
    (p', tc) <- addArgument env p
    x <- mapM (\(x, y) -> (x,) <$> addArgument env y) e
    return (PatternE p' (map (\(x, (y, _)) -> (x, y)) x), tc `union` unions (map (snd . snd) x))
  addArgument _ x = return (x, empty)

  find' :: MonadError String m => [Class] -> [([Class], (String, [Class]))] -> m ([TypedAST], [([Char], Type)])
  find' subCls env = bimap concat concat . unzip <$> mapM
    (\x -> case map (first $ fromRight M.empty) $ filter (isRight . fst) $ map (\z@(cls', _) -> (constraintCheck cls' [x], z)) env of
      -- If a superclass instance exists
      [(s, ([z@(IsIn cls t2)], (name, subCls)))] -> do
        (subVar, subTC) <- find' (tyApply s subCls) env
        let var = VarE name (tyApply s (appify z))
          in return ([if null subVar
            then var
            else AppE var subVar (tyApply s (appify z))], subTC)

      xs -> if containsTVar (appify x)
        then return (map (\z@(IsIn cls tys) ->
        VarE (cls ++ createTypeInstName tys) (appify z)) subCls,
        map (\z@(IsIn cls tys) ->
          (cls ++ createTypeInstName tys, appify z)) subCls)
        else 
          if null xs
            then throwError $ "No instance found for " ++ show x
            else throwError $ "Instances " ++ intercalate ", " (map (\(_, (x:xs, _)) -> show x) xs) ++ " overlaps for " ++ show x) subCls

  addDictOnRecursive :: TypedAST -> (String, Set (String, Type)) -> TypedAST
  addDictOnRecursive (VarE n t) (name, tc) = if n == name
    then AppE (VarE name t) (map (uncurry VarE) $ toList tc) t
    else VarE n t
  addDictOnRecursive (AppE e1 e2 t) z = AppE (addDictOnRecursive e1 z) (map (`addDictOnRecursive` z) e2) t
  addDictOnRecursive (LetE n v) z = LetE n (addDictOnRecursive v z)
  addDictOnRecursive (LetInE n v e) z = LetInE n (addDictOnRecursive v z) (addDictOnRecursive e z)
  addDictOnRecursive (PatternE p e) z = PatternE (addDictOnRecursive p z) (map (second (`addDictOnRecursive` z)) e)
  addDictOnRecursive (AbsE args body) z = AbsE args (addDictOnRecursive body z)
  addDictOnRecursive x _ = x