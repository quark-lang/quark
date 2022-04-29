{-# LANGUAGE StandaloneDeriving #-}
module Core.Inference.Type.Pretty where
  import Core.Color (bold, bBlack, bGreen, bYellow, bBlue, bCyan)
  import Core.Inference.Type.AST
    (Scheme(..), Type(..), TypedAST(LitE, LetE, AbsE, AppE, VarE))
  import qualified Core.Parser.AST as A

  instance Show TypedAST where
    show = flip showAST 0

  createIndent :: Int -> String
  createIndent n = replicate n ' '

  showAST :: TypedAST -> Int -> String
  showAST (LetE (name, t) body) b 
    = bBlue "let " ++ name ++ " = " ++ showAST body b ++ "\n"
  showAST (AbsE (n, t) body) n' = 
    let i1    = createIndent n'
        body' = showAST body (n' + 2)
      in i1 ++ bBlue "lambda " ++ n ++ " : " ++ show t ++ bBlack " ->\n" ++ body'
  showAST (AppE n arg t) i =
    let i1 = createIndent i
      in i1 ++ bBlack "[" ++ showAST n 0 ++ bBlack "] " ++ showAST arg 0 
  showAST (VarE n t) i =
    let i1 = createIndent i
      in i1 ++ n ++ " : " ++ show t
  showAST (LitE (A.String s) _) i 
    = createIndent i ++ bGreen (show s) 
  showAST (LitE (A.Integer i) _) i1
    = createIndent i1 ++ bYellow (show i)
  showAST (LitE (A.Float f) _) i1
    = createIndent i1 ++ bYellow (show f)
  showAST x _ = error "Pattern not recognized in showAST"

  parens :: String -> String
  parens s = bBlack "(" ++ s ++ bBlack ")"

  showTy :: Type -> (Bool, Bool) -> String
  showTy (TApp (TId "List") a) b = bBlack "[" ++ showTy a b ++ bBlack "]"
  showTy (TVar n) _ = bBlack $ "t" ++ show n
  showTy (TId s) _ = bold s
  showTy (t1 :-> t2) (b1, b2) =
    let s = showTy t1 (not b1, b2) ++ bBlack " -> " ++ showTy t2 (not b1, b2)
      in if b1 then parens s else s
  showTy Int _ = bCyan "Int"
  showTy Float _ = bCyan "Float"
  showTy String _ = bCyan "String"
  showTy (TApp t1 t2) (b1, b2) =
    let s = showTy t1 (b1, not b2) ++ " " ++ showTy t2 (b1, not b2)
      in if b2 then parens s else s

  instance Show Type where
    show = flip showTy (False, False)

  deriving instance Show Scheme