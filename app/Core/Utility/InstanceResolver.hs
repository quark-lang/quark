module Core.Utility.InstanceResolver where
  import Core.Inference.Type.AST (TypedAST(..), Type(..), Class(..))
  import Core.Inference.Type.Pretty (show')
  import Control.Arrow (Arrow(second, first))
  import Debug.Trace (traceShow)
  import Core.Inference.Type.Methods (Types(tyUnify, tyApply), constraintCheck, tyCompose)
  import Data.Either (isRight, isLeft, fromRight)
  import Data.Set (Set, toList, fromList, empty, union, unions)
  import Core.Inference.Type (createTypeInstName, createInstName)
  import qualified Data.Map as M

  containsTVar :: Type -> Bool
  containsTVar (TVar _) = True
  containsTVar (TApp t1 t2) = containsTVar t1 || all containsTVar t2
  containsTVar (t1 :-> t2) = all containsTVar t1 || containsTVar t2
  containsTVar _ = False

  addArgument :: Bool -> [([Class], (String, [Class]))] -> TypedAST -> (TypedAST, Set (String, Type))
  addArgument b e (AbsE args body) = do
    let (args', tc) = addArgument b e body
    if null tc || b
      then (AbsE args args', tc)
      else (AbsE (toList tc) (AbsE args args'), tc)
  addArgument b e (AppE (InstE n t) e2 t') =
    case t of
      cls :=> t' ->
        let argsTy = map (\(IsIn cls ty) -> TApp (TId cls) ty) cls
            argsNm = map (\(IsIn cls ty) -> cls ++ createTypeInstName ty) cls
            cls'   = map (\(IsIn cls ty) -> (cls ++ createTypeInstName ty, TApp (TId cls) ty)) cls
            args   = zipWith (curry (\(name, arg@(TApp (TId cls'') tys)) -> if containsTVar arg
              then ([VarE (cls'' ++ createTypeInstName tys) arg], [])
              else case map (first (fromRight M.empty)) $ filter (isRight . fst) $ map (\z@(cls', (_, sub)) -> (constraintCheck (sub ++ cls') cls, z)) e of
                [(s, (cls, (name, subCls)))] ->
                  (map (uncurry VarE) cls' ++ map (\(IsIn cls' ty) -> VarE (cls' ++ createTypeInstName (tyApply s ty)) (TApp (TId cls') (tyApply s ty))) subCls, 
                  map (\(IsIn cls ty) -> (cls ++ createTypeInstName (tyApply s ty), TApp (TId cls) (tyApply s ty))) subCls)
                _ -> ([VarE (cls'' ++ createTypeInstName tys) arg], []))) argsNm argsTy
            arguments = concatMap fst args
            classes   = concatMap snd args
          in (AppE (AppE (VarE n t) arguments t') (e2) t', fromList (cls' ++ classes))
      _ -> (AppE (VarE n t) e2 t', empty)
  addArgument b e (AppE e1 e2 t') = do
    let (e1', tc1) = addArgument b e e1
        t = map (addArgument b e) e2
    (AppE e1' (map fst t) t', tc1 `union` unions (map snd t))
  addArgument b e (LetE n@(name, _) v) = do
    let (b', tc) = addArgument (name == "main") e v
    (LetE n b' , tc)
  addArgument b env (LetInE n v e) = do
    let (v', _) = addArgument b env v
    let (e', tc) = addArgument b env e
    (LetInE n v' e', tc)
  addArgument b env (InstE n t) = (VarE n t, empty)
  addArgument b _ x = (x, empty)
