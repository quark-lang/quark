{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Closure where
  import Core.Parser.AST (AST(..))
  import Core.Parser.Utils.Garbage (removeDuplicates, toList, fromList)
  import Control.Monad.State

  {-
    Module: Lambda lifting
    Description: Passing lambda environment through their heading arguments and
                 partially calling them.
    Author: thomasvergne
  -}

  newtype Data = Data {
    variables :: [String]
  } deriving Show

  type Scope = State Data

  addVariable :: String -> Scope ()
  addVariable var = modify (\(Data vars) -> Data (var:vars))

  removeVariable :: String -> Scope ()
  removeVariable var = modify (\(Data vars) -> Data (filter (/=var) vars))

  removeVariables :: [String] -> Scope ()
  removeVariables = mapM_ removeVariable

  getVariables :: Scope [String]
  getVariables = gets variables

  diff :: Eq a => [a] -> [a] -> [a]
  diff xs ys = filter (\x -> not (elem x ys)) xs

  fromLiterals :: [AST] -> [String]
  fromLiterals = map (\case
    Literal s -> s
    _ -> error "fromLiterals: not a literal")
  
  convertClosure :: AST -> AST
  convertClosure a = evalState (convertClosure' a) (Data [])
    where 
      convertClosure' (Node (Literal "fn") (args:body:_)) = do
        let args'   = fromLiterals (toList args)
        newVars <- getVariables

        mapM addVariable args' -- adding the variables to the scope
        body' <- convertClosure' body
        removeVariables args' -- removing the variables from the scope

        let newFnArgs = fromList $ map Literal (newVars ++ args')
        
        let newFn = Node (Literal "fn") [newFnArgs, body']

        -- additionnally adding environment arguments call
        return $ (Node newFn $ map Literal newVars)

      convertClosure' (Node (Literal "let") (Literal var:value:_)) = do
        -- reducing value before adding variable because variable shouldn't
        -- be accessed from the value
        value' <- convertClosure' value
        addVariable var
        
        -- rebuilding the let expression
        return $ Node (Literal "let") [Literal var, value']

      -- Converting standard node is just in reducing recursively and then
      -- removing difference from old scope and new scope
      convertClosure' (Node n xs) = do
        oldVars <- getVariables

        n'  <- convertClosure' n
        xs' <- mapM convertClosure' xs 

        newVars <- getVariables
        removeVariables (diff newVars oldVars)

        return $ Node n' xs'

      convertClosure' x = return x
