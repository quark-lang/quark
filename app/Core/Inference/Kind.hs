{-# LANGUAGE FlexibleInstances #-}
module Core.Inference.Kind where
  import qualified Data.Map as M
  import Control.Monad.State (MonadIO, modify, MonadState(get))
  import Control.Monad.RWS (MonadRWS, MonadReader(local, ask))
  import qualified Core.Parser.AST as A

  data Kind
    = Star
    | Kind :~> Kind
    | KVar Int
    deriving (Eq, Ord)

  instance Show Kind where
    show Star = "*"
    show (k1 :~> k2) = "(" ++ show k1 ++ " -> " ++ show k2 ++ ")"
    show (KVar i) = "k" ++ show i
  
  type KindEnv = M.Map String Kind
  type SubKy = M.Map Int Kind
  type MonadKind m = (MonadRWS KindEnv () Int m, MonadIO m)

  class Kinds a where
    kyFree :: a -> [Int]
    kyApply :: SubKy -> a -> a
    kyUnify :: a -> a -> SubKy

  kyFresh :: MonadKind m => m Kind
  kyFresh = get >>= \n -> modify (+1) >> return (KVar n)

  kyCompose :: SubKy -> SubKy -> SubKy
  kyCompose s1 s2 = M.map (kyApply s1) s2 `M.union` s1

  merge :: Ord k => [M.Map k a] -> M.Map k a
  merge = foldl (M.unionWith (\_ b -> b)) M.empty

  kyVariableHelper :: Int -> Kind -> SubKy
  kyVariableHelper n t
    | t == KVar n = M.empty
    | n `elem` kyFree t = error "occurs check failed"
    | otherwise = M.singleton n t

  instance Kinds Kind where
    kyFree Star = []
    kyFree (k1 :~> k2) = kyFree k1 ++ kyFree k2
    kyFree (KVar k) = [k]

    kyApply sub (KVar k) = M.findWithDefault (KVar k) k sub
    kyApply sub (k1 :~> k2) = kyApply sub k1 :~> kyApply sub k2
    kyApply _   Star = Star

    kyUnify (KVar k) t = kyVariableHelper k t
    kyUnify t (KVar k) = kyVariableHelper k t
    kyUnify (l :~> r) (l' :~> r') =
      let s1 = kyUnify l l'
          s2 = kyUnify (kyApply s1 r) (kyApply s1 r')
        in s1 `kyCompose` s2
    kyUnify Star Star = M.empty
    kyUnify _ _ = error "kinds do not unify"
  instance Kinds a => Kinds [a] where
    kyFree = concatMap kyFree
    kyApply = map . kyApply
    kyUnify a b = merge $ zipWith kyUnify a b
  instance Kinds KindEnv where
    kyFree = kyFree . M.elems
    kyApply = M.map . kyApply
    kyUnify = error "kinds do not unify"

  kyInfer :: MonadKind m => A.AST -> m (SubKy, Kind)
  kyInfer _ = error "Not implemented yet"
