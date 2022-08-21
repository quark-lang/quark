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
  import Data.List (nub)
  
  containsTVar :: Type -> Bool
  containsTVar (TVar _) = True
  containsTVar (TApp t1 t2) = containsTVar t1 || all containsTVar t2
  containsTVar (t1 :-> t2) = all containsTVar t1 || containsTVar t2
  containsTVar _ = False

  buildFun :: TypedAST -> [TypedAST] -> TypedAST
  buildFun t [] = t
  buildFun t (x:xs) = AppE (buildFun t xs) [x] (getType x)

  fromApp :: Type -> Class
  fromApp (TApp (TId cls) tys) = IsIn cls tys
  fromApp _ = error "fromApp: not a class"

  addArgument :: (Bool, [(String, Class)]) -> [([Class], (String, [Class]))] -> TypedAST -> (TypedAST, Set (String, Type))
  addArgument z@(b, env) e a@(AbsE args body) = do
    let env' = map (second fromApp) $ filter (isInstanceName . fst) args
    let (args', tc) = addArgument (b, env ++ env') e body
    (AbsE args args', tc)
  addArgument b e (AppE e1 e2 t') = do
    let (e1', tc1) = addArgument b e e1
        t = map (addArgument b e) e2
    (AppE e1' (map fst t) t', tc1 `union` unions (map snd t))
  addArgument (_, names) e (LetE n@(name, _) v) = do
    let (b', tc) = addArgument (name == "main", names) e v
    (LetE n b' , tc)
  addArgument b env (LetInE n@(name, _) v e) = do
    let (v', _) = addArgument b env v
    let (e', tc) = addArgument b env e
    (LetInE n v' e', tc)
  addArgument (_, env') env (InstE n t) = case t of
    cls :=> ty ->
      let argsTy = map (\(IsIn cls ty) -> TApp (TId cls) ty) cls
          argsNm = map (\(IsIn cls ty) -> cls ++ createTypeInstName ty) cls
          cls'   = map (\(IsIn cls ty) -> (cls ++ createTypeInstName ty, TApp (TId cls) ty)) cls
          args   = zipWith (curry (\(name, arg@(TApp (TId cls'') tys)) ->
            -- filtering classes to get the correct matching class
            case map (first $ fromRight M.empty) $ filter (isRight . fst) $ map (\z@(cls', _) -> (constraintCheck cls' cls, z)) env of
              [(s, ([a@(IsIn cls_ t1)], (name_, subCls)))] ->
                if null subCls
                  then (VarE name_ (appify a), [], appify a)
                  else case filter (isRight . constraintCheck (tyApply s subCls) . fst) env of
                    -- If a superclass instance exists
                    [([z@(IsIn cls t2)], (name, subCls))] ->
                      (VarE name_ (appify a), [VarE name (appify z)], appify a)

                    -- If no superclass instance exists
                    _ -> case filter (\(n, x) -> isRight $ constraintCheck subCls [x]) env' of
                      [(name, subCls)] -> (VarE name_ (appify a), [VarE name (appify subCls)], appify a)
                      _ -> error $ "No superclass instance found for " ++ show subCls ++ " in " ++ show env'
                      --(VarE name_ (appify a), map (\z@(IsIn cls tys) -> 
                      --VarE (cls ++ createTypeInstName tys) (appify z)) subCls, 
                      -- appify a)

              -- If no class instance exists, then create a generic local parameter one
              _ -> case filter (\(n, x) -> isRight $ constraintCheck cls [x]) env' of
                [(name, cls)] -> (VarE name (appify cls), [], appify cls)
                _ -> error $ "No instance found for " ++ n ++ " |- " ++ show env')) argsNm argsTy
              --(VarE (cls'' ++ createTypeInstName tys) arg, [], arg))
          calls  = map (\(cls, sup, ty) -> if null sup then cls else AppE cls sup ty) args
        in (AppE (VarE n ty) calls ty, empty)
    _ -> error $ n ++ " should have a class constraint"
  addArgument b env (PatternE p e) = do
    let (p', tc) = addArgument b env p
    let x = map (second (addArgument b env)) e
    (PatternE p (map (\(x, (y, _)) -> (x, y)) x), tc `union` unions (map (snd . snd) x))
  addArgument b _ x = (x, empty)
