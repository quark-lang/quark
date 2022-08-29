{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Core.Inference.Type where
  import qualified Data.Map as M
  import qualified Core.Parser.AST as A
  import Control.Monad.RWS
  import Data.Foldable (foldlM, find)
  import Control.Monad (unless)
  import Core.Inference.Type.AST
  import Core.Inference.Type.Pretty
  import Core.Inference.Type.Methods
  import Core.Inference.Type.Parsing
  import System.Exit
  import Data.Maybe (fromMaybe)
  import Core.Utility.Color (red, bBlack, bold)
  import Control.Monad.Except (runExceptT, MonadError (throwError))
  import Data.Either (fromLeft, isLeft, fromRight)
  import GHC.Float (double2Float)
  import Debug.Trace (traceShow)
  import Control.Arrow (Arrow(second, first))
  import Data.List (nub, union, intercalate)
  import Data.Char (isUpper)
  import Data.Bifunctor (Bifunctor(bimap))

  tyPattern :: MonadType m => A.Expression -> m (TypedPattern, SubTy, Type, M.Map String Scheme)
  tyPattern (A.Identifier "_") = do
    t <- tyFresh
    return (WilP t, M.empty, t, M.empty)
  tyPattern (A.Identifier "true") = return (VarP "true" Bool, M.empty, Bool, M.empty)
  tyPattern (A.Identifier "false") = return (VarP "false" Bool, M.empty, Bool, M.empty)
  tyPattern (A.Identifier n) = ask >>= \(Env t c) -> case M.lookup n c of
    Just t -> do
      t' <- tyInstantiate t
      return (VarP n t', M.empty, t', M.empty)
    Nothing -> do
      t <- tyFresh
      return (VarP n t, M.empty, t, M.singleton n (Forall [] t))
  tyPattern z@(A.Node e@(A.Identifier n) xs) = do
    tv <- tyFresh
    (n', s1, t1, m1) <- tyPattern e
    (x', s2, t2, m2) <- foldlM (\(x', s, t, m) x -> do
      (x'', s', t', m') <- local (applyTypes (tyApply s)) $ tyPattern x
      return (x' ++ [x''], s `tyCompose` s', t ++ [t'], m' `M.union` m)) ([], s1, [], M.empty) xs
    case tyUnify (tyApply s2 t1) (t2 :-> tv) of
      Right s3 -> do
        let x'' = tyApply s3 tv
        return (AppP n x' x'', s3 `tyCompose` s2 `tyCompose` s1, x'', m1 `M.union` m2)
      Left x -> throwError (x, z)

  tyPattern (A.Literal (A.String s)) = return (LitP (S s) String, M.empty, String, M.empty)
  tyPattern (A.Literal (A.Integer i)) = return (LitP (I i) Int, M.empty, Int, M.empty)
  tyPattern (A.Literal (A.Float f)) = return (LitP (F f) Float, M.empty, Float, M.empty)
  tyPattern (A.Literal (A.Char c)) = return (LitP (C c) Char, M.empty, Char, M.empty)
  tyPattern x = error $ "tyPattern: not implemented => " ++ show x

  patUnify :: Type -> [Type] -> Either String SubTy
  patUnify x = foldl (\acc y -> tyCompose <$> tyUnify x y <*> acc) (Right M.empty)

  instances :: MonadType m => m Instances
  instances = gets snd

  trimap :: (a -> b) -> (c -> d) -> (e -> f) -> (a, c, e) -> (b, d, f)
  trimap f g h (a, c, e) = (f a, g c, h e)

  find' :: MonadError (String, A.Expression) m => A.Expression -> [Class] -> [([Class], (String, [Class]))] -> m ([TypedAST], [([Char], Type)], [Class])
  find' a subCls env = trimap concat concat concat . unzip3 <$> mapM
    (\x -> case map (first $ fromRight M.empty) $ filter (isRight . fst) $ map (\z@(cls', _) -> (constraintCheck cls' [x], z)) env of
      -- If a superclass instance exists
      [(s, ([z@(IsIn cls t2)], (name, subCls')))] -> do
        (subVar, subTC, cls) <- find' a (tyApply s subCls') env
        --liftIO $ print s
        let var = VarE name (tyApply s (appify z))
          in return ([if null subVar
            then var
            else AppE var subVar (tyApply s (appify z))], subTC, cls)

      xs -> if containsTVar' (appify x)
        then return (map (\z@(IsIn cls tys) ->
          VarE (cls ++ createTypeInstName tys) (appify z)) subCls,
          map (\z@(IsIn cls tys) ->
            (cls ++ createTypeInstName tys, appify z)) subCls, [x])
        else
          if null xs
            then throwError ("No instance found for " ++ show x, a)
            else throwError ("Instances " ++ intercalate ", " (map (\(_, (x:xs, _)) -> show x) xs) ++ " overlaps for " ++ show x, a)) subCls

  -- Main type inference function
  tyInfer :: MonadType m => A.Expression -> m (TypedAST, SubTy, Type)
  -- Type inference for variables
  tyInfer (A.Identifier n) = ask >>= \(Env t c) -> case M.lookup n (t `M.union` c) of
    Just t -> do
      t' <- tyInstantiate t
      return (VarE n t', M.empty, t')
    Nothing -> throwError ("Variable " ++ bold n ++ " is not defined.", A.Identifier n)

  tyInfer z@(A.Node (A.Identifier "match") (pat:cases)) = do
    (pat', s1, pat_t) <- tyInfer pat

    let patterns = map (\(A.List [p, _]) -> p) cases
    let exprs    = map (\(A.List [_, c]) -> c) cases
    let cases'   = zip patterns exprs

    res <- forM cases' $ \(pattern, expr) -> do
      (p, s, t, m) <- tyPattern pattern
      let s2 = s `tyCompose` s1
      (e, s', t') <- local (applyTypes $ tyApply s2 . (m `M.union`)) $ tyInfer expr
      let s3 = s' `tyCompose` s2
      return (tyApply s3 t, tyApply s3 t', s3, (tyApply s3 p, tyApply s3 e))

    if null res
      then (PatternE pat' [], M.empty,) <$> tyFresh
      else do
        let (_, t, _, _) = head res
        let s = foldl (\acc (tp, te, s, _) ->
                let r = tyCompose <$> tyUnify t te <*> tyUnify tp pat_t
                    r' = tyCompose <$> r <*> acc
                  in tyCompose <$> r' <*> pure s) (Right s1) res
        let s2 = foldl (\acc (tp, te, s, _) ->
                let r = tyCompose <$> tyUnify t te <*> tyUnify tp pat_t
                    r' = tyCompose <$> r <*> acc
                  in tyCompose <$> r' <*> pure s) (Right s1) $ reverse res
        let s' = tyCompose <$> s <*> s2
        let tys = map (\(x, _, _, _) -> case s of
                    Right s -> tyApply s x
                    Left _ -> x) res
        let s'' = foldl (\acc x -> tyCompose <$> acc <*> patUnify x tys) (Right M.empty) tys
        case tyCompose <$> s' <*> s'' of
          Right s -> do
            let patterns' = map (\(_, _, _, (x, y)) -> (tyApply s x, tyApply s y)) res
            return (PatternE (tyApply s pat') patterns', s, tyApply s t)
          Left e -> throwError (e, z)

  -- Type inference for abstractions
  tyInfer a@(A.Node (A.Identifier "fn") [A.List args, body]) = do
    tvs <- mapM (const tyFresh) args
    let args' = map unliteral args
    Env env _ <- ask
    let env'  = foldl (flip M.delete) env args'
        env'' = env' `M.union` M.fromList [(x, Forall [] tv) | (x, tv) <- zip args' tvs]
    (b', s1, t1) <- local (applyTypes (const env'')) $ tyInfer body
    let argTy = tyApply s1 tvs
    let argTy' = nub $ concatMap (\t -> let res = concatMap (appearsInTC' b') $ getTVars t
                in if not (null res) then res else []) argTy
    --liftIO $ print argTy'
    -- found <- mapM (\x -> do
    --   (subVar, subTC) <- find' a [x] =<< instances
    --   return (x, (subVar, subTC))) argTy'
    -- let clss = concatMap (\(cls, (ast, f)) -> if null f then [(cls, ast)] else []) found
    -- let args = unzip $ map (\(cls, (_, arg)) -> (cls, arg)) found
    return (tyApply s1 $ AbsE (zip args' argTy) b' , s1, argTy' :=> (argTy :-> t1))

  -- Type inference for let-polymorphic expressions
  tyInfer z@(A.Node (A.Identifier "let") [A.Identifier name, value, body]) = do
    -- Fresh type for recursive definitions
    tv <- tyFresh
    let e =  M.singleton name (Forall [] tv)
    (v', s1, t1) <- local (applyTypes (`M.union` e)) $ tyInfer value
    Env env _ <- ask
    --liftIO $ print (name, t1)

    -- Typechecking variable type
    (s3, t3) <- case M.lookup name env of
          Just t' -> do
            t'' <- tyInstantiate t'
            case tyUnify t1 t'' of
              Right s -> do
                let s' = s `tyCompose` s1
                let r = tyApply s' t1
                return (s', r)
              Left x -> throwError (x, z)
          Nothing -> return (s1, t1)
    --liftIO $ print (name, t1, v'')
    --let t3 = case t2 of
    --      _ :=> ty -> if null preds then ty else preds :=> ty
    --      _ -> if null preds then t2 else preds :=> t2

    let env'  = M.delete name env
        t'    = generalize (tyApply s3 env) (tyApply s3 t3)
        env'' = M.insert name t' env'

    (b', s2, t2) <- local (applyTypes . const $ tyApply s3 env'') $ tyInfer body
    let s4 = s2 `tyCompose` s3
    return (LetInE (name, tyApply s4 t3) (tyApply s4 $ applyClassOnRecursive v' (name, t3)) $ tyApply s4 b', s4, tyApply s4 t2)

  -- Type inference errors
  tyInfer z@(A.Node (A.Identifier "let") _)
    = throwError ("Cannot have let as last expression in block", z)

  tyInfer z@(A.Node (A.Identifier "data") _)
    = throwError ("Cannot have nested data constructor", z)

  tyInfer z@(A.Node (A.Identifier "declare") _)
    = throwError ("Cannot have nested declaration", z)

  -- Type inference for applications
  tyInfer z@(A.Node n xs) = do
    tv <- tyFresh
    (n', s1, t1) <- tyInfer n
    (x', s2, t2) <- foldlM (\(x', s, t) x -> do
      (x'', s', t') <- local (applyTypes (tyApply s)) $ tyInfer x
      return (x' ++ [x''], s `tyCompose` s', t ++ [t'])) ([], s1, []) xs
    let cls' = concatMap (\case
                cls :=> _ -> cls
                _ -> []) t2
    case t1 of
      cls :=> ty -> do
        case tyUnify (tyApply s2 t1) (t2 :-> tv) of
          Right s3 -> do
            let s4 = s3 `tyCompose` s2 `tyCompose` s1
            let x'' = tyApply s4 tv
            return (AppE (tyApply s4 n') (tyApply s4 x') x'', s4, tyApply s4 x'')
          Left x -> throwError (x, z)
      _ ->
        case tyUnify (tyApply s2 t1) (t2 :-> tv) of
          Right s3 -> do
            let s4 = s3 `tyCompose` s2 `tyCompose` s1
            let x'' = tyApply s4 tv
            let app = AppE (tyApply s4 n') (tyApply s4 x') x''
            return (tyApply s4 $ case getType app of
                cls :=> ty -> setType app (nub cls :=> ty)
                _ -> if null cls'
                  then app
                  else setType app (cls' :=> getType app),
                s4, tyApply s4 x'')
          Left x -> throwError (x, z)
  -- Value related inference
  tyInfer (A.Literal (A.String s))  = return (LitE (S s) String, M.empty, String)
  tyInfer (A.Literal (A.Integer i)) = return (LitE (I i) Int, M.empty, Int)
  tyInfer (A.Literal (A.Float f))   = return (LitE (F f) Float, M.empty, Float)
  tyInfer (A.Literal (A.Char c))    = return (LitE (C c) Char, M.empty, Char)
  tyInfer a = throwError ("Unknown expression", a)

  getTVars :: Type -> [Type]
  getTVars (TApp t xs) = getTVars t ++ getTVars xs
  getTVars (TVar x) = [TVar x]
  getTVars (t1 :-> t2) = concatMap getTVars t1 ++ getTVars t2
  getTVars _ = []

  setType :: TypedAST -> Type -> TypedAST
  setType (LitE l t) t' = LitE l t'
  setType (VarE n t) t' = VarE n t'
  setType (AbsE xs e) t' = AbsE xs (setType e t')
  setType (AppE e x _) t' = AppE e x t'
  setType (LetInE (x, t) e b) t' = LetInE (x, t') e (setType b t')
  setType x _ = x

  containsTVar :: Int -> Type -> Bool
  containsTVar x (TVar x') = x == x'
  containsTVar i (TApp t1 t2) = containsTVar i t1 || containsTVar i t2
  containsTVar i (t1 :-> t2) = all (containsTVar i) t1 || containsTVar i t2
  containsTVar i _ = False

  appearsInTC :: Type -> Type -> [Class]
  appearsInTC (TVar t) (ps :=> _) = filter (\(IsIn _ p) -> any (containsTVar t) p) ps
  appearsInTC t (t1 :-> t2) = concatMap (appearsInTC t) t1 ++ appearsInTC t t2
  appearsInTC _ _ = []

  appearsInTC' :: TypedAST -> Type -> [Class]
  appearsInTC' (LitE _ t) t' = appearsInTC t' t
  appearsInTC' (VarE _ t) t' = appearsInTC t' t
  appearsInTC' (AbsE t b) t' = concatMap ((`appearsInTC` t') . snd) t ++ appearsInTC' b t'
  appearsInTC' (LetInE (_, t) v b) t' = appearsInTC t t' ++ appearsInTC' v t' ++ appearsInTC' b t'
  appearsInTC' (AppE e x t) t' = appearsInTC' e t' ++ concatMap (`appearsInTC'` t') x ++ appearsInTC t' t
  appearsInTC' (PatternE e ps) t' = appearsInTC' e t' ++ concatMap ((`appearsInTC'` t') . snd) ps
  appearsInTC' (ListE xs _) t = concatMap (`appearsInTC'` t) xs
  appearsInTC' (LetE (_, t) b) t' = appearsInTC' b t' ++ appearsInTC t' t
  appearsInTC' (DataE _ _) _ = []

  createInstName :: [Class] -> String
  createInstName (IsIn name ty:ts) = name ++ createTypeInstName ty ++ createInstName ts
  createInstName [] = ""

  containsTVar' :: Type -> Bool
  containsTVar' (TVar _) = True
  containsTVar' (TApp t1 t2) = containsTVar' t1 || containsTVar' t2
  containsTVar' (t1 :-> t2) = all containsTVar' t1 || containsTVar' t2
  containsTVar' _ = False

  createTypeInstName :: [Type] -> String
  createTypeInstName (t:ts) = case t of
    TVar x -> show x ++ (if null ts then "" else "_") ++ createTypeInstName ts
    TApp n ts -> createTypeInstName [n] ++ createTypeInstName [ts]
    TId n -> n
    _ -> show t ++ createTypeInstName ts
  createTypeInstName _ = ""

  appify :: Class -> Type
  appify (IsIn c ty) = buildData (TId c) ty

  isInstanceName :: String -> Bool
  isInstanceName (c:_) = isUpper c
  isInstanceName [] = False

  resolveInstances :: MonadType m => TypedAST -> m (TypedAST, [(String, Type)], [Class])
  resolveInstances (LitE l t) = return (LitE l t, [], [])
  resolveInstances (VarE n t) = case t of
    cls :=> ty -> do
      env <- instances
      (calls, tcs, preds) <- find' (A.Identifier n) cls env
      return (if not (null calls) then AppE (VarE n (preds :=> ty)) calls ty else VarE n t, tcs, preds)
    _ -> return (VarE n t, [], [])
  resolveInstances (AppE e x t) = do
    (e', tcs, cls) <- resolveInstances e
    (x', tcs', cls') <- unzip3 <$> mapM resolveInstances x
    return (AppE e' x' t, tcs ++ concat tcs', cls ++ concat cls')
  resolveInstances (AbsE xs e) = do
    (e', tcs, cls) <- resolveInstances e
    return (AbsE xs e', tcs, cls)
  resolveInstances (LetInE (x, t) e b) = do
    (e', tcs, cls) <- resolveInstances e
    let t1 = case t of
          _ :=> ty -> if null cls then ty else cls :=> ty
          _ -> if null cls then t else cls :=> t
    (b', tcs', cls') <- resolveInstances b
    return (LetInE (x, t1) (if null tcs then e' else AbsE (nub tcs) e') b', tcs', cls')
  resolveInstances (PatternE e ps) = do
    (e', tcs, cls) <- resolveInstances e
    (ps', tcs', cls') <- unzip3 <$> mapM (\(p, e) -> do
      (e', tcs, cls) <- resolveInstances e
      return ((p, e'), tcs, cls)) ps
    return (PatternE e' ps', tcs ++ concat tcs', cls ++ concat cls')
  resolveInstances x = return (x, [], [])

  applyClassOnRecursive :: TypedAST -> (String, Type) -> TypedAST
  applyClassOnRecursive z@(VarE n _) (n', t)
    | n == n' = VarE n t
    | otherwise = z
  applyClassOnRecursive (AppE e x t) (n, t') = AppE (applyClassOnRecursive e (n, t')) (map (`applyClassOnRecursive` (n, t')) x) t
  applyClassOnRecursive (AbsE xs e) (n, t) = AbsE xs (applyClassOnRecursive e (n, t))
  applyClassOnRecursive (LetInE (x, t) v b) (n, t') = LetInE (x, t) (applyClassOnRecursive v (n, t')) (applyClassOnRecursive b (n, t'))
  applyClassOnRecursive (PatternE e ps) (n, t) = PatternE (applyClassOnRecursive e (n, t)) (map (\(p, e) -> (p, applyClassOnRecursive e (n, t))) ps)
  applyClassOnRecursive x _ = x
  

  topLevel :: MonadType m => A.Expression -> m (Maybe [TypedAST], Env)
  topLevel (A.Node (A.Identifier "class") [name', A.List fields]) = do
    let (name, tyArgs) = parseTypeHeader name'

    argsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs
    let cls = IsIn name (M.elems argsMap)
    let fields' = map (\case
                (A.Node (A.Identifier "declare") [dat, _]) -> parseTypeHeader dat
                _ -> error "Not a identifier") fields
    ty <- mapM (\((name, ty), t) -> case t of
      (A.Node (A.Identifier "declare") [_, t]) -> do
        argsMap' <- M.union argsMap . M.fromList <$> mapM ((`fmap` tyFresh) . (,)) ty
        let ty = parseType argsMap' t
        Env e _ <- ask
        return (name, case ty of
          clss :=> ty -> (cls : clss) :=> ty
          _ -> [cls] :=> ty)
      _ -> error "Not a declaration") (zip fields' fields)
    e@(Env t _) <- ask
    let cons = map (\(name, ty) -> case ty of
            cls :=> (args :-> t) -> (args) :-> t
            cls :=> args -> args
            _ -> ty) ty
    let datType = buildData (TId name) $ M.elems argsMap
    let patterns = AppP name (map (uncurry VarP) ty) datType
    return (
      Just $ DataE (name, M.elems argsMap) [(name, cons :-> datType)] :
      map (\(name, ty) -> LetE (name, [datType] :-> ty) (AbsE [("$s", datType)] (PatternE (VarE "$s" datType) [(patterns, VarE name ty)]))) ty,
      applyCons (`M.union` M.fromList (map (second $ generalize t) ty)) e)

  topLevel (A.Node (A.Identifier "instance") [A.List instances, A.Node (A.Identifier cls) xs, A.List fields]) = do
    (env, ty) <- parseInstanceHeader M.empty xs
    x <- mapM (\(A.Node (A.Identifier cls) xs) -> (cls,) <$> parseInstanceHeader env xs) instances
    let subClasses = map (\(cls, (_, ty)) -> IsIn cls ty) x

    let cls' = IsIn cls ty
    let name = createInstName [cls']

    fields' <- mapM (\case
      z@(A.Node (A.Identifier "let") [A.Identifier name', value]) -> do
        ask >>= \(Env _ t) -> case M.lookup name' t of
          Just ty' -> do
            -- create an instance of the method
            t' <- tyInstantiate ty'

            (v', s1, t1) <- tyInfer value
            -- adding classes to the inferred type
            let t1' = case t1 of
                      cls :=> ty -> (cls `union` (cls' : subClasses)) :=> ty
                      _ -> (cls' : subClasses) :=> t1
            -- unifying it with instantatied method type
            let s2 = tyUnify t' t1'
            case s2 of
              Right s -> do
                --liftIO $ putStrLn $ show' t'
                --liftIO $ putStrLn $ show' t1
                --liftIO $ print s
                --liftIO $ putStrLn $ show' (tyApply s t1)
                --liftIO $ putStrLn ""
                Env _ e <- ask
                -- returning inferred type, inferred value, inferred method scheme and substitution
                return ((tyApply s t1, tyApply s v'), M.singleton name' (generalize e (tyApply s t1)), s)
              Left x -> throwError (x, z)
          Nothing -> throwError ("Unknown variable", z)
      x -> throwError ("Unknown method", x)) fields

    let tys = map (\((ty, _), _, _) -> ty) fields'
    let fields'' = map (\((_, f), _, _) -> f) fields'

    let s = map (\(_, _, s) -> s) fields'
    let s' = foldl1 tyCompose s

    freshInstance (tyApply s' [cls'], (name, tyApply s' subClasses))

    (fields'', args, tys) <- unzip3 <$> forM (zip fields'' tys) (\(v', t1') -> do
      (v'', args, preds) <- resolveInstances v'
      let t1 = case t1' of
            _ :=> ty -> if null preds then ty else preds :=> ty
            _ -> if null preds then t1' else preds :=> t1'
      return (v'', args, t1))

    let datType = buildData (TId cls) $ tyApply s' ty
    let ty' = (cls' : subClasses) :=> (tys :-> datType)
    let call = AppE (VarE cls ty') fields'' datType

    let subConstraints = map (\(cls, (_, ty)) -> buildData (TId cls) $ tyApply s' ty) x
    let subNames = map (\(cls, (_, ty)) -> cls ++ createTypeInstName (tyApply s' ty)) x
    let args' = nub $ concat args
    return (Just [LetE (name, datType) (if null args' then call else AbsE args' call)], emptyEnv)
  -- Empty data constructor (just a phantom type)
  topLevel (A.Node (A.Identifier "data") [dat]) = do
    (constr', ast) <- parseData (parseTypeHeader dat) (A.List [])
    e <- ask
    return (Just [ast], applyCons (`M.union` constr') e)

  -- Top-level data with constructors
  topLevel (A.Node (A.Identifier "data") [dat, constructors]) = do
    (constr', ast) <- parseData (parseTypeHeader dat) constructors
    e <- ask
    return (Just [ast], applyCons (`M.union` constr') e)

  -- Top-level let expression (let-in shouldn't be used in top-level)
  topLevel z@(A.Node (A.Identifier "let") [A.Identifier name, value]) = do
    -- Fresh type for recursive definitions
    tv <- tyFresh
    let e =  M.singleton name (Forall [] tv)
    (v', s1, t1') <- local (applyTypes (`M.union` e)) $ tyInfer value
    Env env c <- ask
    --liftIO $ print v'
    (v'', args, preds) <- resolveInstances v'
    let t1 = case t1' of
          _ :=> ty -> if null preds then ty else preds :=> ty
          _ -> if null preds then t1' else preds :=> t1'
    -- Typechecking variable type
    (s3, t2) <- case M.lookup name env of
          Just t' -> do
            t'' <- tyInstantiate t'
            case tyUnify t1 t'' of
              Right s -> do
                let r = tyApply s t''
                return (s `tyCompose` s1, r)
              Left x -> throwError (x, z)
          Nothing -> return (s1, t1)
    let env'  = M.delete name env
        t'    = generalize (tyApply s3 env) (tyApply s3 t2)
        env'' = M.insert name t' env'
    --liftIO $ print name
    --liftIO $ putStrLn $ show' $ tyApply s3 t1
    --liftIO $ print (name, tyApply s3 t2)
    return (Just [LetE (name, tyApply s3 t2) (tyApply s3 $ if not (null args) then AbsE args v'' else v'')], Env env'' c)

  -- Top-level declare used to define function type
  topLevel z@(A.Node (A.Identifier "declare") [dat, def]) = do
    let (name, tyArgs) = parseTypeHeader dat

    argsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs

    let ty = parseType argsMap def

    e@(Env t _) <- ask
    --kindsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs
    --((_, b), _, _) <- runRWST (kyCheck def) (k `M.union` M.map (\(TVar k) ---> KVar k) kindsMap) 0

    --liftIO $ print (b, name)
    return (Nothing, applyTypes (`M.union` M.singleton name (generalize t ty)) e)
  -- topLevel z@(A.Node (A.Identifier "declare") [A.List instances, dat, def]) = do
  --   let (name, tyArgs) = parseTypeHeader dat
  --   let instances' = map parseTypeHeader instances
  --   let instArgs   = concatMap snd instances'

  --   argsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) (tyArgs `union` instArgs)

  --   let constraints = map (\(cls, ty) -> IsIn cls $ 
  --         map (\x -> M.findWithDefault (TId x) x argsMap) ty) instances'

  --   let ty = parseType argsMap def

  --   e@(Env t _) <- ask
  --   return (Nothing, applyTypes (`M.union` M.singleton name (generalize t (constraints :=> ty))) e)
  topLevel x = throwError ("Unknown top-level expression received", x)

  runInfer :: MonadIO m => [A.Expression] -> m (Either (String, A.Expression) ([TypedAST], [([Class], (String, [Class]))]))
  runInfer a =
    fmap fst <$> foldlM (\e x -> case e of
      Right ((a, w), e) -> do
        x <- runExceptT $ runRWST (topLevel x) e (0, w)
        case x of
          Right ((a', e'), (_, w'), _) -> return $ Right (case a' of
            Nothing -> (a, w)
            Just a' -> (a ++ a', nub $ w ++ w'), mergeEnv e e')
          Left err -> return $ Left err
      Left err -> return $ Left err) (Right (([], []), emptyEnv)) a