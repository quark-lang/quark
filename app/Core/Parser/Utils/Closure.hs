{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Closure where
  import Core.Parser.AST (AST(..))
  import Core.Parser.Utils.Garbage (removeDuplicates, toList)

  {-
    Module: Lambda lifting
    Description: Converting closured functions to normal functions and putting them at the top of the scope and passing their environment to the arguments 
    Author: thomasvergne
  -}

  newtype Data = Data {
    variables :: [String]
  } deriving Show

  addVariable :: String -> Data -> Data
  addVariable var (Data vars) = Data (var:vars)

  addVariables :: [String] -> Data -> Data
  addVariables vars (Data vars') = Data (vars ++ vars')

  getLiterals :: AST -> [AST] -> [AST]
  getLiterals (Literal "Nil") xs = xs
  getLiterals x@(Literal _) xs = x:xs
  getLiterals (Node (Literal "fn") _) xs = xs
  getLiterals (Node _ xs) xs' = foldl (flip getLiterals) xs' xs
  getLiterals _ xs = xs

  confront :: [AST] -> [String] -> [AST]
  confront = foldl (\ xs y -> filter (\ (Literal x) -> x /= y) xs)

  getVariablesNotInScope :: AST -> Data -> [AST]
  getVariablesNotInScope s d = removeDuplicates $ confront (getLiterals s []) (variables d)

  replaceVariables :: [AST] -> AST -> AST
  replaceVariables vars (Node n xs) = Node n (map (replaceVariables vars) xs)
  replaceVariables vars x@(Literal _) =
    if x `elem` vars
      then Node (Literal "env") [x]
      else x
  replaceVariables _ x = x

  getArgName :: AST -> String
  getArgName (Literal n) = n
  getArgName _ = error "Argument must be a literal!"

  convertClosure :: (AST, Data) -> AST -> AST
  convertClosure (p, d) (Node (Literal "begin") xs)
    = Node (Literal "begin") (map (convertClosure (p, d)) xs)
  convertClosure (p, d) (Node (Literal "fn") (args:body))
    = do
      -- collecting variables name in function scope
      let vars = 
            foldl (\acc -> \case
              Node (Literal "let") [Literal name, _] -> addVariable name acc
              _ -> acc) d body
              
      -- adding to data arguments as variable
      let args' = addVariables (map getArgName (toList args)) vars

      -- replacing environment variables
      let vars' = 
            getVariablesNotInScope 
              (Node (Literal "begin") body) args'
          body' = map (\x -> let a = replaceVariables vars' x in convertClosure (a, Data []) a) body
        in Node (Literal "fn") (args : body')
  convertClosure (p, d) (Node n xs)
    = Node (convertClosure (p, d) n) (map (convertClosure (p, d)) xs)
  convertClosure _ x = x
