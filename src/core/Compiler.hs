{-# LANGUAGE LambdaCase, BlockArguments #-}
module Core.Compiler where
  import Core.Parser
  import Control.Monad.State
  import Control.Monad (when)
  import Data.List
  import Data.Sequence (update, fromList)
  import Data.Foldable (toList)
  import qualified Data.Map as M

  -- VM Types

  type Frame    = [Value]
  type Stack    = [Frame]

  type Variable = (String, Value)
  type Table    = M.Map String Value



  data Value
    = VString String
    | VInteger Integer
    | VDouble Double
    | VBool Bool
    | VList [Value]
    | VCode [Bytecode] (Frame, Table)
    deriving (Show, Eq, Ord)
  data Bytecode
    = PUSH Value
    | STORE String
    | LOAD String
    | LOAD_SEGMENT Int
    | PRINT Int
    | CALL Int
    | DROP String
    | JUMP Int Int
    | LIST Int

    | ADD
    | MUL
    | SUB
    | DIV

    -- Comparison instructions
    | CMP_EQ
    | CMP_GT
    | CMP_LT
    deriving (Show, Ord, Eq)

  type Page = [Bytecode]
  type Address = Int
  type Segment = (Address, Page)

  type Name = String
  type Scope = [Name]

  type Program = (Address, [Page], [Scope])

  newScope :: State Program ()
  newScope = state \(a,p,s) -> ((), (a, p, [] : s))

  variable :: String -> State Program ()
  variable v = state \(a,p,x:xs) -> ((), (a,p, (x ++ [v]) : xs))

  removeScope :: State Program Scope
  removeScope = state \(a,p,x:xs) -> (x, (a, p, xs))

  scopes :: State Program [Scope]
  scopes = state \z@(_,_,s) -> (s, z)

  push :: Bytecode -> State Program ()
  push instr = do
    (address, pages, s) <- get
    let page = pages !! address
    let updated = toList $ update address (page ++ [instr]) $ fromList pages
    put (address, updated, s)

  increment :: State Program ()
  increment = state \(_,p, s) -> ((), (length p, p ++ [[]], s))

  set :: Address -> State Program ()
  set i = state \(_,p, s) -> ((), (i, p, s))

  next :: State Program Address
  next = state \z@(_,p,_) -> (length p, z)

  address :: State Program Address
  address = state \z@(i,_,_) -> (i, z)

  compile :: Atom ->  State Program ()
  compile (Expression z@(x:xs)) = case x of
    Word "=" -> mapM_ compile xs >> push CMP_EQ
    Word "-" -> mapM_ compile xs >> push SUB
    Word "+" -> mapM_ compile xs >> push ADD
    Word "*" -> mapM_ compile xs >> push MUL
    Word "print" -> mapM_ compile xs >> push (PRINT (length xs))

    Word "list" -> mapM_ compile xs >> push (LIST $ length xs)

    Word "fn" -> do
      Control.Monad.when (length xs < 2) $ error "You must specify arguments and body to a function"
      let (args:body:_) = xs
      addr <- address
      i <- next
      push $ LOAD_SEGMENT i -- Loading future segment
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
      case name of
        Word x -> compile value >> push (STORE x) >> variable x
        z -> error $ "Variable name must be a word, received a " ++ show z

    Word "begin" -> do
      newScope
      mapM_ compile xs
      scope <- removeScope
      unless (null scope) $ mapM_ (push . DROP) scope

    Word "if" -> do
      let (cond:then':else':_) = xs
      addr <- address
      compile cond

      thenAddr <- next
      increment
      compile then'

      set addr

      elseAddr <- next
      increment
      compile else'

      set addr
      push $ JUMP thenAddr elseAddr

    -- Compiling function call
    Word x -> do
      push (LOAD x)
      mapM_ compile xs
      push (CALL (length xs))

    -- Compiling anonymous function
    Expression z@(Word "fn":_) -> do
      compile x
      mapM_ compile xs
      push $ CALL (length xs)

    _ -> mapM_ compile z

  compile (Integer i) = push $ PUSH (VInteger i)
  compile (String s)  = push $ PUSH (VString s)
  compile (Double d)  = push $ PUSH (VDouble d)
  compile (Word w)    = push $ LOAD w
  compile _ = return ()

  -- Useful functions

  initProgram :: Program
  initProgram = (0, [[]], [[]]) -- First element represents global scope