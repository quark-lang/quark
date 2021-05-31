{-# LANGUAGE LambdaCase, BlockArguments #-}
module Core.Compiler where
  import Core.Parser
  import Control.Monad.State
  import Data.List
  import Data.Sequence (update, fromList)
  import Data.Foldable (toList)

  data Value
    = VString String
    | VInteger Integer
    | VDouble Double
    | VList [Value]
    | VLambda [Bytecode]
    deriving (Show, Eq, Ord)
  data Bytecode
    = PUSH Value
    | STORE String
    | LOAD String
    | LOAD_SEGMENT Integer
    | PRINT
    | CALL Int
    | DROP String
    deriving (Show, Ord, Eq)

  type Page = [Bytecode]
  type Address = Int
  type Segment = (Address, Page)

  type Variable = String
  type Scope = [Variable]

  type Program = (Address, [Page])

  push :: Bytecode -> State Program ()
  push instr = do
    (address, pages) <- get
    let page = pages !! address
    let updated = toList $ update address (page ++ [instr]) $ fromList pages
    put (address, updated)

  increment :: State Program ()
  increment = state \(_,p) -> ((), (length p, p ++ [[]]))

  set :: Address -> State Program ()
  set i = state \(_,p) -> ((), (i, p))

  next :: State Program Address
  next = state \z@(_,p) -> (length p, z)

  address :: State Program Address
  address = state \z@(i,_) -> (i, z)

  compile :: Atom ->  State Program ()
  compile (Expression z@(x:xs)) = case x of
    Word "fn" -> do
      let (args:body:_) = xs
      addr <- address
      i <- next
      push $ LOAD_SEGMENT $ toInteger i -- Loading future segment
      increment -- Incrementing segment pointer

      case args of
        Expression xs -> mapM_ (\case
          Word x -> push $ STORE x
          z -> error $ "Argument must be a word, received a " ++ show z) xs
        _ -> error "Arguments must be an atom!"

      compile body
      set addr

    Word "let" -> do
      let (name:value:_) = xs
      compile value
      case name of
        Word x -> push $ STORE x
        z -> error $ "Variable name must be a word, received a " ++ show z
    
    Word "begin" -> mapM_ compile xs

    -- Compiling function call
    Word x -> do
      push (LOAD x)
      mapM_ compile xs
      push (CALL (length xs))

    -- Compiling anonymous function
    Expression z@(Word "fn":_) -> compile x >> mapM_ compile xs >> push (CALL (length xs))

    _ -> mapM_ compile z

  compile (Integer i) = push (PUSH (VInteger i))
  compile (String s) = push (PUSH (VString s))
  compile _ = return ()

  -- Useful functions

  initProgram :: Program
  initProgram = (0, [[]]) -- First element represents global scope