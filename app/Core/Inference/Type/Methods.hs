{-# LANGUAGE FlexibleInstances #-}
module Core.Inference.Type.Methods where
  import Core.Inference.Type.AST
  import Core.Inference.Type.Pretty ()
  import qualified Data.Map as M
  import qualified Data.Set as S
  import Control.Monad.RWS (modify, MonadState(get), MonadIO (liftIO))
  import Data.Bifunctor (Bifunctor(second, bimap))
  import Debug.Trace (traceShow)

  -- Some environment functions and types
  applyTypes :: (TypeEnv -> TypeEnv) -> Env -> Env
  applyTypes f (Env ty cons) = Env (f ty) cons

  applyCons :: (ConsEnv -> ConsEnv) -> Env -> Env
  applyCons f (Env ty cons) = Env ty (f cons)

  emptyEnv :: Env
  emptyEnv = Env M.empty M.empty

  mergeEnv :: Env -> Env -> Env
  mergeEnv (Env t c) (Env t' c')
    = Env (t `M.union` t') (c `M.union` c')

  -- Substitution composition is a way of merging two substitutions
  tyCompose :: SubTy -> SubTy -> SubTy
  tyCompose s1 s2 = M.map (tyApply s1) s2 `M.union` s1

  setConcatMap :: Ord b => (a -> S.Set b) -> [a] -> S.Set b
  setConcatMap f = S.unions . map f

  class Types a where
    tyFree  :: a -> S.Set Int
    tyApply :: SubTy -> a -> a
    tyUnify :: a -> a -> Either String SubTy 

  -- Unification variable helper
  variable :: Int -> Type -> Either String SubTy
  variable n t
    | t == TVar n = Right M.empty
    | n `elem` tyFree t = Left $ "Occurs check failed: " ++ show t
    | otherwise = Right $ M.singleton n t

  instance Types Type where
    tyFree (TVar i) = S.singleton i
    tyFree (t1 :-> t2) = tyFree t1 `S.union` tyFree t2
    tyFree Int = S.empty
    tyFree String = S.empty
    tyFree (ListT t) = tyFree t
    tyFree (TApp t1 t2) = tyFree t1 `S.union` tyFree t2
    tyFree _ = S.empty

    tyApply s (TVar i) = case M.lookup i s of
      Just t -> t
      Nothing -> TVar i
    tyApply s (t1 :-> t2) = tyApply s t1 :-> tyApply s t2
    tyApply s (ListT t) = ListT $ tyApply s t
    tyApply s (TApp t1 t2) = TApp (tyApply s t1) (tyApply s t2)
    tyApply _ s = s

    tyUnify (TVar i) t = variable i t
    tyUnify t (TVar i) = variable i t
    tyUnify (t1 :-> t2) (t3 :-> t4)
      = let s1 = foldl (\acc (t, t') -> case tyUnify t t' of
                          Right s -> tyCompose <$> acc <*> pure s
                          Left s -> Left s) (Right M.empty) $ zip t1 t3
          in tyCompose <$> s1 <*> tyUnify t2 t4
    tyUnify (TId s) (TId s') = if s == s'
      then Right M.empty
      else Left $ "Type " ++ s ++ " mismatches with type " ++ s'
    tyUnify _ Any = Right M.empty
    tyUnify Any _ = Right M.empty
    tyUnify (ListT t) (ListT t') = tyUnify t t'
    tyUnify Int Int = Right M.empty
    tyUnify String String = Right M.empty
    tyUnify Bool Bool = Right M.empty
    tyUnify (TApp t1 t2) (TApp t3 t4) =
      let s1 = tyUnify t1 t3
          s2 = foldl (\acc (t, t') -> case tyUnify t t' of
                          Right s -> tyCompose <$> acc <*> pure s
                          Left s -> Left s) (Right M.empty) $ zip t2 t4
        in tyCompose <$> s1 <*> s2
    tyUnify s1 s2 = Left $ "Type " ++ show s1 ++ " mismatches with type " ++ show s2
  instance Types Scheme where
    tyFree (Forall v t) = tyFree t S.\\ S.fromList v
    tyApply s (Forall v t) = Forall v (tyApply (foldr M.delete s v) t)
    tyUnify _ _ = error "Cannot unify type scheme"
  instance Types a => Types [a] where
    tyFree = setConcatMap tyFree
    tyApply = map . tyApply
    tyUnify _ _ = error "Cannot unify type scheme"
  instance Types TypeEnv where
    tyFree = tyFree . M.elems
    tyApply = M.map . tyApply
    tyUnify = error "Cannot unify type environment"

  instance Types TypedAST where
    tyFree (AppE f arg t) = tyFree f `S.union` tyFree arg
    tyFree (AbsE n body) = tyFree (map snd n) `S.union` tyFree body
    tyFree (VarE name t) = tyFree t
    tyFree (LetInE (name, t) body t')
      = (tyFree t S.\\ tyFree t) `S.union` tyFree body `S.union` tyFree t'
    tyFree (LetE (name, t) body) = (tyFree t S.\\ tyFree t) `S.union` tyFree body
    tyFree (ListE args t) = tyFree t `S.union` setConcatMap tyFree args
    tyFree (LitE ast t) = tyFree t
    tyFree _ = S.empty

    tyApply s (AppE f arg t) = AppE (tyApply s f) (tyApply s arg) (tyApply s t)
    tyApply s (AbsE n body) = AbsE (map (second (tyApply s)) n) (tyApply s body)
    tyApply s (VarE name t) = VarE name (tyApply s t)
    tyApply s (LetInE (name, t) body t') = LetInE (name, tyApply s t) (tyApply s body) (tyApply s t')
    tyApply s (ListE args t) = ListE (map (tyApply s) args) (tyApply s t)
    tyApply s (LetE (name, t) body) = LetE (name, tyApply s t) (tyApply s body)
    tyApply s (LitE ast t) = LitE ast (tyApply s t)
    tyApply s (PatternE pat t) = PatternE (tyApply s pat) (map (bimap (tyApply s) (tyApply s)) t)
    tyApply s (DataE name args) = DataE name (map (second $ tyApply s) args)

    tyUnify _ _ = error "Cannot unify AST"

  instance Types TypedPattern where
    tyFree (VarP _ t) = tyFree t
    tyFree (WilP t) = tyFree t
    tyFree (AppP p1 p2 t) = tyFree p2
    tyFree _ = S.empty

    tyApply s (VarP n t) = VarP n (tyApply s t)
    tyApply s (WilP t) = WilP (tyApply s t)
    tyApply s (AppP p1 p2 t) = AppP p1 (tyApply s p2) (tyApply s t)
    tyApply _ p = p

    tyUnify _ _ = error "Cannot unify pattern"

  -- Generalization is the process of managing flexible type variables
  -- as rigid type variables.
  generalize :: TypeEnv -> Type -> Scheme
  generalize env t = Forall (S.toList vars) t
    where vars = tyFree t S.\\ tyFree env

  -- Creating a new fresh type variable
  tyFresh :: MonadType m => m Type
  tyFresh = get >>= \n -> modify (+1) >> return (TVar n)

  -- Create a type instance based on a scheme
  tyInstantiate :: MonadType m => Scheme -> m Type
  tyInstantiate (Forall vars t) = do
    vars' <- mapM (const tyFresh) vars
    let s = M.fromList $ zip vars vars'
      in return $ tyApply s t