{-# LANGUAGE FlexibleInstances #-}
module Core.Inference.Kind where
  import qualified Data.Map as M
  import Control.Monad.State (MonadIO, modify, MonadState(get), liftIO)
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

    kyApply sub (KVar k) = case M.lookup k sub of
      Just t -> kyApply sub t
      Nothing -> KVar k
    kyApply sub (k1 :~> k2) = kyApply sub k1 :~> kyApply sub k2
    kyApply _   Star = Star

    kyUnify (KVar k) t = kyVariableHelper k t
    kyUnify t (KVar k) = kyVariableHelper k t
    kyUnify (l :~> r) (l' :~> r') =
      let s1 = kyUnify l l'
          s2 = kyUnify (kyApply s1 r) (kyApply s1 r')
        in s1 `kyCompose` s2
    kyUnify Star Star = M.empty
    kyUnify k1 k2 = error $ "Kind " ++ show k1 ++ " and " ++ show k2 ++ " cannot be unified"
  instance Kinds a => Kinds [a] where
    kyFree = concatMap kyFree
    kyApply = map . kyApply
    kyUnify a b = merge $ zipWith kyUnify a b
  instance Kinds KindEnv where
    kyFree = kyFree . M.elems
    kyApply = M.map . kyApply
    kyUnify = error "kinds do not unify"

  buildKindFun :: [Kind] -> Kind
  buildKindFun = foldr1 (:~>)

  removeNArguments :: Int -> Kind -> Kind
  removeNArguments n (k1 :~> k2) = removeNArguments (n - 1) k2
  removeNArguments 0 k = k 
  removeNArguments n _ = error "removeNArguments: not enough arguments"

  kyCheck :: MonadKind m => A.AST -> m (SubKy, Kind)
  kyCheck (A.Literal "*") = return (M.empty, Star)
  kyCheck (A.Literal "str") = return (M.empty, Star)
  kyCheck (A.Literal "int") = return (M.empty, Star)
  kyCheck (A.Literal n) = ask >>= \env -> case M.lookup n env of
    Just t -> return (M.empty, t)
    Nothing -> error $ "unknown type: " ++ show n
  kyCheck (A.Node (A.Literal "->") xs) = do
    r <- mapM kyCheck xs
    let es = merge (map fst r)
        ts = map snd r
    return (es, kyApply es $ buildKindFun ts)
  kyCheck (A.Node n xs) = do
    (e, kn) <- kyCheck n
    r <- mapM (local (kyApply e) . kyCheck) xs
    let es   = merge (map fst r)
        ts   = map snd r
        env  = e `kyCompose` es
        fun  = buildKindFun (ts ++ [Star])
        uni  = kyUnify fun kn
        env' = uni `kyCompose` env
    liftIO $ print (n, fun)
    return (env', removeNArguments (length r) $ kyApply env' fun)
  kyCheck x = error $ "unknown type: " ++ show x