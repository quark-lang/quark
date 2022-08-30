{-# LANGUAGE LambdaCase #-}
module Core.Inference.Type.Parsing where
  import qualified Data.Map as M
  import qualified Core.Parser.AST as A
  import Core.Inference.Type.AST
  import Control.Monad (forM, unless)
  import Core.Inference.Type.Methods (tyFresh, trim)
  import Debug.Trace (traceShow)
  import Data.List (union)

  buildDataType :: String -> [Type] -> Type
  buildDataType name args = if null args then TId (trim name) else buildData (TId $ trim name) args

  isRight :: Either a b -> Bool
  isRight (Right _) = True
  isRight _ = False

  right :: Either a b -> b
  right (Right x) = x
  right (Left _) = error "Not a right"

  parseConstructor :: Type -> M.Map String Type -> [A.Expression] -> Type
  parseConstructor d m xs
    = let xs' = map (parseType m) xs
        in foldr (:->) d [xs']

  buildFun :: [Type] -> Type
  buildFun [] = error "Empty function"
  buildFun [x] = x
  buildFun (x:xs) = [x] :-> buildFun xs

  parseType :: M.Map String Type -> A.Expression -> Type
  parseType e (A.Node (A.Identifier "->") [y]) = [] :-> parseType e y
  parseType e (A.Node (A.Identifier "->") xs) = buildFun (map (parseType e) xs)
  parseType e (A.Node n xs) =
    let xs' = map (parseType e) xs
        n'  = parseType e n
      in buildData n' xs'
  parseType e (A.List [x]) = TApp (TId "List") (parseType e x)
  parseType e (A.Identifier "str") = TApp (TId "List") Char
  parseType e (A.Identifier "bool") = Bool
  parseType e (A.Identifier "int") = Int
  parseType e (A.Identifier "char") = Char
  parseType e (A.Identifier "float") = Float
  parseType e (A.Identifier n) = case M.lookup n e of
    Nothing -> TId n
    Just i  -> i
  parseType _ _ = error "Invalid constructor"

  unliteral :: A.Expression -> String
  unliteral (A.Identifier i) = i
  unliteral _ = error "Not an identifier"

  parseTypeHeader :: A.Expression -> (String, [String])
  parseTypeHeader (A.Node (A.Identifier name) args) = (name, map unliteral args)
  parseTypeHeader (A.Identifier name) = (name, [])
  parseTypeHeader _ = error "Invalid type header"

  parseInstanceHeader :: MonadType m => M.Map String Type -> [A.Expression] -> m (M.Map String Type, [Type])
  parseInstanceHeader e (t:ts) = do
    (names, types) <- parseInstanceHeader e ts
    case t of
      A.Identifier "str" -> return (names, TApp (TId "List") Char : types)
      A.Identifier "bool" -> return (names, Bool : types)
      A.Identifier "int" -> return (names, Int : types)
      A.Identifier "char" -> return (names, Char : types)
      A.Identifier "float" -> return (names, Float : types)
      A.Identifier name -> do
        case M.lookup name (names `M.union` e) of
          Nothing -> do
            if head name `elem` ['A'..'Z'] 
              then return $ (names, TId name : types)
              else do
                ty <- tyFresh
                return (M.insert name ty names, ty : types)
          Just ty -> return (names, ty : types)
      (A.Node (A.Identifier "->") [y]) -> do
        (names', ty) <- parseInstanceHeader e [y]
        case ty of
          [ty'] -> return (names', ty' : types)
          _ -> error "Invalid type"
      (A.Node (A.Identifier "->") xs) -> do
        (names', ty) <- parseInstanceHeader e xs
        return (names `M.union` names', (init ty :-> last ty) : types)
      (A.Node n xs) -> do
        (n2, xs') <- parseInstanceHeader e xs
        (n1, n')  <- parseInstanceHeader e [n]
        case n' of
          [n''] -> return (n1 `M.union` n2, (buildData n'' xs') : types)
          _ -> error "Invalid type"
      _ -> error "Invalid type"
  parseInstanceHeader _ _ = return (M.empty, [])

  parseData :: MonadType m => (String, [String]) -> A.Expression -> m (TypeEnv, TypedAST)
  parseData (name, tyArgs) (A.List constructors) = do
    argsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs
    let tyVars   = map (argsMap M.!) tyArgs
        dataType = buildDataType name tyVars
        schemeV  = map (\(TVar n) -> n) tyVars
        schemeCt = Forall schemeV

    constr' <- forM constructors $ \case
      A.Node (A.Identifier name) args -> do
        let consTy = parseConstructor dataType argsMap args
          in return (trim name, consTy)
      A.Identifier name -> return (trim name, dataType)
      _ -> error "Invalid constructor"

    return (M.map schemeCt (M.fromList constr'), DataE (trim name, tyVars) constr')
  parseData _ _ = error "Invalid data type"
