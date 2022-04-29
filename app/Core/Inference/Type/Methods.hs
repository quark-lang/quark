{-# LANGUAGE FlexibleInstances #-}
module Core.Inference.Type.Methods where
  import Core.Inference.Type.AST
    ( MonadType,
      Scheme(..),
      ConsEnv,
      TypeEnv,
      SubTy,
      Env(..),
      Type((:->), TId, Int, String, TApp, TVar),
      TypedAST(..) )
  import Core.Inference.Type.Pretty ()
  import Core.Inference.Kind (KindEnv)
  import qualified Data.Map as M
  import Data.List ((\\))
  import Control.Monad.RWS (modify, MonadState(get))

  -- Some environment functions and types
  applyTypes :: (TypeEnv -> TypeEnv) -> Env -> Env
  applyTypes f (Env ty cons k) = Env (f ty) cons k

  applyCons :: (ConsEnv -> ConsEnv) -> Env -> Env
  applyCons f (Env ty cons k) = Env ty (f cons) k

  applyKinds :: (KindEnv -> KindEnv) -> Env -> Env
  applyKinds f (Env ty cons k) = Env ty cons (f k)

  emptyEnv :: Env
  emptyEnv = Env M.empty M.empty M.empty

  mergeEnv :: Env -> Env -> Env
  mergeEnv (Env t c f) (Env t' c' f') 
    = Env (t `M.union` t') (c `M.union` c') (f `M.union` f')

  -- Substitution composition is a way of merging two substitutions
  tyCompose :: SubTy -> SubTy -> SubTy
  tyCompose s1 s2 = M.map (tyApply s1) s2 `M.union` s1

  class Types a where
    tyFree  :: a -> [Int]
    tyApply :: SubTy -> a -> a
    tyUnify :: a -> a -> SubTy

  -- Unification variable helper
  variable :: Int -> Type -> SubTy
  variable n t
    | t == TVar n = M.empty
    | n `elem` tyFree t = error $ "Occurs check failed: " ++ show t
    | otherwise = M.singleton n t

  instance Types Type where
    tyFree (TVar i) = [i]
    tyFree (t1 :-> t2) = tyFree t1 ++ tyFree t2
    tyFree Int = []
    tyFree String = []
    tyFree (TApp t1 t2) = tyFree t1 ++ tyFree t2
    tyFree _ = []

    tyApply s (TVar i) = case M.lookup i s of
      Just t -> tyApply s t
      Nothing -> TVar i
    tyApply s (t1 :-> t2) = tyApply s t1 :-> tyApply s t2
    tyApply s (TApp t1 t2) = TApp (tyApply s t1) (tyApply s t2)
    tyApply _ s = s

    tyUnify (TVar i) t = variable i t
    tyUnify t (TVar i) = variable i t
    tyUnify (t1 :-> t2) (t3 :-> t4) =
      let s1 = tyUnify t1 t3
          s2 = tyUnify (tyApply s1 t2) (tyApply s1 t4)
        in s1 `tyCompose` s2
    tyUnify (TId s) (TId s') = if s == s'
      then M.empty
      else error $ "Type " ++ s ++ " mismatches with type " ++ s'
    tyUnify Int Int = M.empty
    tyUnify String String = M.empty
    tyUnify (TApp t1 t2) (TApp t3 t4) = tyUnify t1 t3 `M.union` tyUnify t2 t4
    tyUnify s1 s2 = error $ "Type " ++ show s1 ++ " mismatches with type " ++ show s2
  instance Types Scheme where
    tyFree (Forall v t) = tyFree t \\ v
    tyApply s (Forall v t) = Forall v (tyApply (foldr M.delete s v) t)
    tyUnify _ _ = error "Cannot unify type scheme"
  instance Types a => Types [a] where
    tyFree = concatMap tyFree
    tyApply = map . tyApply
    tyUnify _ _ = error "Cannot unify type scheme"
  instance Types TypeEnv where
    tyFree = tyFree . M.elems
    tyApply = M.map . tyApply
    tyUnify = error "Cannot unify type environment"
  instance Types TypedAST where
    tyFree (AppE f arg t) = tyFree f ++ tyFree arg
    tyFree (AbsE (n, t) body) = (tyFree t \\ tyFree t) ++ tyFree body
    tyFree (VarE name t) = tyFree t
    tyFree (LetInE (name, t) body t') = (tyFree t \\ tyFree t) ++ tyFree body ++ tyFree t'
    tyFree (LetE (name, t) body) = (tyFree t \\ tyFree t) ++ tyFree body
    tyFree (ListE args t) = tyFree t ++ concatMap tyFree args
    tyFree (LitE ast t) = tyFree t

    tyApply s (AppE f arg t) = AppE (tyApply s f) (tyApply s arg) (tyApply s t)
    tyApply s (AbsE (n, t) body) = AbsE (n, tyApply s t) (tyApply s body)
    tyApply s (VarE name t) = VarE name (tyApply s t)
    tyApply s (LetInE (name, t) body t') = LetInE (name, tyApply s t) (tyApply s body) (tyApply s t')
    tyApply s (ListE args t) = ListE (map (tyApply s) args) (tyApply s t)
    tyApply s (LetE (name, t) body) = LetE (name, tyApply s t) (tyApply s body)
    tyApply s (LitE ast t) = LitE ast (tyApply s t)

    tyUnify _ _ = error "Cannot unify AST"

  -- Generalization is the process of managing flexible type variables
  -- as rigid type variables.
  generalize :: TypeEnv -> Type -> Scheme
  generalize env t = Forall vars t
    where vars = tyFree t \\ tyFree env

  -- Creating a new fresh type variable
  tyFresh :: MonadType m => m Type
  tyFresh = get >>= \n -> modify (+1) >> return (TVar n)

  -- Create a type instance based on a scheme
  tyInstantiate :: MonadType m => Scheme -> m Type
  tyInstantiate (Forall vars t) = do
    vars' <- mapM (const tyFresh) vars
    let s = M.fromList $ zip vars vars'
      in return $ tyApply s t