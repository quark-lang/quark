module Core.Compiler.Definition.Generation where
  import Core.Compiler.Definition.IR (Expression(..))
  import Core.Inference.Type.AST (Literal(..))
  import Data.List
  import Text.Printf (printf)
  import Data.Char (isLetter, ord)
  
  isIdent x = isLetter x || x == '_'

  varify :: String -> String
  varify x = "$" ++ concatMap (\x -> if isIdent x then [x] else show (ord x)) x

  encodeUnicode16 :: String -> String
  encodeUnicode16 = concatMap escapeChar
    where
      escapeChar c
        | ' ' <= c && c <= 'z' = [c]
        | otherwise = printf "\\u%04x" (fromEnum c)
        
  from :: Expression -> String
  from (Lambda args expr@(Object _)) = "(" ++ intercalate "," args ++ ") => (" ++ from expr ++ ")"
  from (Lambda args expr) = "(" ++ intercalate "," args ++ ") => " ++ from expr
  from (Var n) = n
  from (Object props) = "{" ++ intercalate "," (map (\(p, v) -> p  ++ ": " ++ from v) props) ++ "}"
  from (Ternary t c e) = "(" ++ from t ++ " ? " ++ from c ++ " : " ++ from e ++ ")"
  from (Array exprs) = "[" ++ intercalate "," (map from exprs) ++ "]"
  from (Call (Var n) xs) = n ++ "(" ++ intercalate "," (map from xs) ++ ")"
  from (Call z@(Property _ _) xs) = from z ++ "(" ++ intercalate "," (map from xs) ++ ")"
  from (Call n xs) = "(" ++ from n ++ ")(" ++ intercalate "," (map from xs) ++ ")"
  from (Property n z@(Lit _)) = from n ++ "[" ++ from z ++ "]"
  from (Property n p) = from n ++ "." ++ from p
  from (BinaryCall l op r) = from l ++ " " ++ op ++ " " ++ from r
  from (Let n v) = "var " ++ n ++ " = " ++ from v
  from (Condition c t) = "if (" ++ from c ++ ") " ++ from t
  from (Return e) = "return " ++ from e
  from (Block exprs) = "{" ++ concatMap ((++";") . from) exprs ++ "}"
  from (Throw e) = "throw " ++ from e
  from (Lit (S s)) = "\"" ++ encodeUnicode16 s ++ "\""
  from (Lit (F f)) = show f
  from (Lit (I i)) = show i
  from (Require p) = "Object.entries(require(\"" ++ p ++ "\")).map(([name, exported]) => global[name] = exported);"
  from (Raw c) = c
  from (Index e i) = from e ++ "[" ++ from i ++ "]"
  from _ = error "from: not implemented"