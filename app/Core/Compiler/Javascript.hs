{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Compiler.Javascript where
  import Core.Compiler.Compiler (AST(..))
  import Control.Monad.State
  import Data.List

  isBlock :: AST -> Bool
  isBlock (Block _) = True
  isBlock _ = False

  type IsTopBlock = Bool

  endLine :: IsTopBlock -> String
  endLine True = ";"
  endLine False = ""

  from :: AST -> IsTopBlock -> String
  from (Block xs) _ =
    concat ["{", concatMap (`from` True) xs, "}"]

  from (FunctionCall (Identifier "list") els) i =
    concat ["[", intercalate "," $ map (`from` False) els, "]", endLine i]

  from (FunctionCall fn args) i =
    let fnCall = case fn of
          Identifier _ -> from fn False
          _ -> concat ["(", from fn False, ")"]
      in
        concat [fnCall, "(", intercalate "," $ map (`from` False) args, ")", endLine i]

  from (Identifier x) i = x ++ endLine i
  from (String s)     i = show s ++ endLine i
  from (Number n)     i = show n ++ endLine i
  from (Lambda args body) i =
    concat ["(", intercalate "," args, ")", "=>", from body False, endLine i]
  from (BinaryExpression op lhs rhs) i =
    concat [from lhs False, getCorrectOp op, from rhs False, endLine i]
  from (Assignment n v) i =
    concat ["var ", n, "=", from v False, endLine i]
  from (Condition c t e) i =
    if isBlock t || isBlock e
      then concat ["if(", from c False, ") ", from t i, "else ", from e i]
      else concat [from c False, "?", from t False, ":", from e False, endLine i]
  from (Delete n) i = concat ["delete ", from n False, endLine i]

  getCorrectOp :: String -> String
  getCorrectOp "=" = "=="
  getCorrectOp "or" = "||"
  getCorrectOp "and" = "&&"
  getCorrectOp x = x

  compileJavascript :: AST -> String
  compileJavascript = flip from True