{-# LANGUAGE LambdaCase #-}
module Core.Inference.Type.Parsing where
  import qualified Data.Map as M
  import qualified Core.Parser.AST as A
  import Core.Inference.Type.AST
  import Core.Inference.Kind (Kind(Star, (:~>)))
  import Control.Monad (forM, unless)
  import Core.Inference.Type.Methods (tyFresh)
  import Core.Parser.Macros (unliteral)

  buildDataType :: String -> [Type] -> Type
  buildDataType name [] = TId name
  buildDataType name (a:rgs) = foldl TApp (TApp (TId name) a) rgs

  isRight :: Either a b -> Bool
  isRight (Right _) = True
  isRight _ = False

  right :: Either a b -> b
  right (Right x) = x
  right (Left _) = error "Not a right"

  parseConstructor :: Type -> M.Map String Type -> [A.AST] -> Type
  parseConstructor d m xs
    = let xs' = map (parseType m) xs
        in case head xs' of
          Right _ -> foldr ((:->) . right) d (filter isRight xs')
          Left _ -> error "Constructor type mismatch"

  buildFun :: [Either Kind Type] -> Either Kind Type
  buildFun [] = error "Cannot build function type"
  buildFun [t] = t
  buildFun (Left k:ts) = case buildFun ts of
    Left k' -> Left (k :~> k')
    Right _ -> error "Cannot mix kinds and types"
  buildFun (Right t:ts) = case buildFun ts of
    Left _ -> error "Cannot mix kinds and types"
    Right t' -> Right (t :-> t')

  parseType :: M.Map String Type -> A.AST -> Either Kind Type
  parseType e (A.Node (A.Literal "->") xs) = buildFun (map (parseType e) xs)
  parseType e (A.Node n xs) =
    let xs' = map (parseType e) xs
        n'  = parseType e n
      in foldl (\acc x -> TApp <$> acc <*> x) n' xs'
  parseType e (A.List [x]) = case parseType e x of
    Right x' -> Right $ TApp (TId "List") x'
    Left _ -> error "Cannot create kind list"
  parseType e (A.Literal "str") = Right String
  parseType e (A.Literal "bool") = Right Bool
  parseType e (A.Literal "int") = Right Int
  parseType e (A.Literal "any") = Right Any
  parseType e (A.Literal "expr") = Right Expr
  parseType e (A.Literal "*") = Left Star
  parseType e (A.Literal n) = case M.lookup n e of
    Nothing -> Right $ TId n
    Just i  -> Right i
  parseType _ _ = error "Invalid constructor"

  parseTypeHeader :: A.AST -> (String, [String])
  parseTypeHeader (A.Node (A.Literal name) args) = (name, map unliteral args)
  parseTypeHeader (A.Literal name) = (name, [])
  parseTypeHeader _ = error "Invalid type header"

  parseData :: MonadType m => (String, [String]) -> A.AST -> m (TypeEnv, TypedAST)
  parseData (name, tyArgs) (A.List constructors) = do
    argsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs
    let tyVars   = map (argsMap M.!) tyArgs
        dataType = buildDataType name tyVars
        schemeV  = map (\(TVar n) -> n) tyVars
        schemeCt = Forall schemeV

    constr' <- forM constructors $ \case
      A.Node (A.Literal name) args -> do
        let consTy = parseConstructor dataType argsMap args
          in return (name, consTy)
      A.Literal name -> return (name, dataType)
      _ -> error "Invalid constructor"

    return $ (M.map schemeCt (M.fromList constr'), DataE (name, tyVars) constr')
  parseData _ _ = error "Invalid data type"