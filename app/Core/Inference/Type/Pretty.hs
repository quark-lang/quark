{-# LANGUAGE StandaloneDeriving #-}
module Core.Inference.Type.Pretty where
  import Core.Color (bold, bBlack, bGreen, bYellow, bBlue, bCyan)
  import Core.Inference.Type.AST
    (Scheme(..), Type(..), TypedAST(LitE, LetE, AbsE, AppE, VarE, LetInE, ListE, DataE))
  import qualified Core.Parser.AST as A
  import Data.List (intercalate)

  instance Show TypedAST where
    show = flip showAST 0

  createIndent :: Int -> String
  createIndent n = replicate n ' '

  showAST :: TypedAST -> Int -> String
  showAST (LetE (name, t) body) b
    = bBlue "let " ++ name ++ " = " ++ showAST body b ++ "\n"
  showAST (AbsE (n, t) body) n' =
    let i1    = createIndent (n' + 2)
        body' = showAST body (n' + 2)
        lmbd  = bBlue "lambda " ++ n ++ " : " ++ show t ++ bBlack " ->\n" ++ i1
      in lmbd ++ body'
  showAST (AppE n arg t) i =
    bBlack "[" ++ showAST n 0 ++ bBlack "] " ++ showAST arg 0
  showAST (VarE n t) _
    = n ++ " : " ++ show t
  showAST (LitE (A.String s) _) _
    = bGreen (show s)
  showAST (LitE (A.Integer i) _) _
    = bYellow (show i)
  showAST (LitE (A.Float f) _) _
    = bYellow (show f)
  showAST (ListE xs _) _
    = bBlack "[" ++ intercalate "," (map (`showAST` 0) xs) ++ bBlack "]"
  showAST (LetInE (name, t) body e) i
    = bBlue "let " ++ name ++ " = " ++ showAST body i ++ "\n" ++
      createIndent (i + 2) ++ bBlue "in " ++ showAST e 0
  showAST (DataE (name, generics) []) i
    = bBlue "data " ++ bold name ++ " " ++ unwords (map show generics)
  showAST (DataE (name, generics) ((consName, ty):xs)) i =
    bBlue "data " ++ bold name ++ " " ++ unwords (map show generics) ++ "\n" ++
      createIndent (i + 2) ++ "= " ++ consName ++ " :: " ++ show ty ++ "\n" ++
      concatMap (\(n, t) -> createIndent (i + 2) ++ "| " ++ n ++ " :: " ++ show t ++ "\n") xs
  showAST x _ = error "Pattern not recognized in showAST"

  parens :: String -> String
  parens s = bBlack "(" ++ s ++ bBlack ")"

  showTy :: Type -> (Bool, Bool) -> String
  showTy (TApp (TId "List") a) b = bBlack "[" ++ showTy a b ++ bBlack "]"
  showTy (TVar n) _ = bBlack $ "a" ++ show n
  showTy (TId s) _ = bold s
  showTy (t1 :-> t2) (b1, b2) =
    let s = showTy t1 (not b1 || b1, b2) ++ bBlack " -> " ++ showTy t2 (not b1 || b1, b2)
      in if b1 then parens s else s
  showTy Int _ = bCyan "Int"
  showTy Float _ = bCyan "Float"
  showTy String _ = bCyan "String"
  showTy (TApp t1 t2) (b1, b2) =
    let s = showTy t1 (b1, not b2 || b2) ++ " " ++ showTy t2 (b1, not b2 || b2)
      in if b2 then parens s else s

  instance Show Type where
    show = flip showTy (False, False)

  deriving instance Show Scheme