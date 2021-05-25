{-# LANGUAGE LambdaCase, BlockArguments #-}
module Core.Compiler where
  import Core.Parser
  import Control.Monad.State
  import Data.List
  data Value
    = VString String
    | VInteger Integer
    | VDouble Double
    | VList [Value]
    | VLambda [Bytecode]
    deriving Show
  data Bytecode
    = PUSH Value
    | STORE String
    | LOAD String
    | LOAD_SEGMENT Integer
    | PRINT
    | CALL Int
    | DROP String
    deriving Show

  type Page = [Bytecode]
  type Address = Integer
  type Segment = (Address, Page)

  type Variable = String
  type Scope = [Variable]

  type SegmentPointer = Integer
  type Program = (SegmentPointer, [Segment], [Scope])

  -- Bytecode modifiers section
  spanBytecode :: State Program (Segment, [Segment]) -- (CurrentEntry, OtherEntries)
  spanBytecode = do
    (i,segs,_) <- get
    entry <- getCurrentEntry
    let removedEntry = filter ((/=i) . fst) segs
    return (entry, removedEntry)

  push :: Bytecode -> State Program ()
  push instr = do
    (current,others) <- spanBytecode
    let pushed = let (addr, bc) = current in (addr, bc ++ [instr])
    state \(i,_,s) -> ((), (i, others ++ [pushed],s))

  -- Entry setters section
  newEntry :: State Program ()
  newEntry = state \(i',entries,s) -> ((), (i', (toInteger $ length entries, []) : entries,s))

  -- Entry getters section
  getMainEntry :: State Program Segment
  getMainEntry = state \p@(_,segs,_) -> (last segs, p)

  getCurrentEntry :: State Program Segment
  getCurrentEntry = state \p@(i,segs,s) -> do
    case findEntryByPointer i segs of
      Just x -> (x, p)
      Nothing -> error $ "Can't load " ++ show i ++ " segment!" ++ show p

  findEntryByPointer :: SegmentPointer -> [Segment] -> Maybe Segment
  findEntryByPointer i = find ((==i) . fst)

  -- Pointer moves section
  incSegmentPointer :: State Program ()
  incSegmentPointer = state \(i,p,s) -> ((), (i + 1, p, s))

  decSegmentPointer :: State Program ()
  decSegmentPointer = state \(i,p,s) -> ((), (i - 1, p,s))

  movSegmentPointer :: SegmentPointer -> State Program ()
  movSegmentPointer i = state \(_,p,s) -> ((), (i, p,s))

  getCurrentPointer :: State Program SegmentPointer
  getCurrentPointer = state \p@(i,_,_) -> (i, p)

  -- Compilation section  

  popVariableScope :: State Program Scope
  popVariableScope = state \(i,p,s) -> (last s, (i, p, init s))

  pushVariable :: String -> State Program ()
  pushVariable var = state \(i,p,s) -> ((), (i, p, case s of
    [] -> []
    _ -> let (head:rest) = s in (var : head) : rest))

  pushNewScope :: State Program ()
  pushNewScope = state \(i,p,s) -> ((), (i, p, [] : s))


  compile :: Atom -> State Program ()
  compile (Expression (x:xs)) = case x of
    Word "fn" -> let (args:body:_) = xs in do
      newEntry
      incSegmentPointer

      case args of
        (Expression args') -> mapM_ (\case
          Word wrd -> push $ STORE wrd
          _ -> error "Function argument must be a word!")  args'
        _ -> error "Function arguments must be encapsuled in an atom!"

      compile body
      index <- getCurrentPointer
      decSegmentPointer
      push (LOAD_SEGMENT index)

    Word "begin" -> do
      pushNewScope
      mapM_ compile xs
      variables <- popVariableScope
      mapM_ (push . DROP) variables
      
    Word "print" -> mapM_ compile xs >> push PRINT
    Word "let" -> do
      let (name:value:_) = xs
      compile value
      case name of
        Word str -> pushVariable str >> push (STORE str)
        _ -> error "Variable name must be a word !"
    
    Word "drop" -> let (name:_) = xs in case name of
      Word wrd -> push $ DROP wrd
      String str -> push $ DROP str
      _ -> error "Cannot drop list or integer or double variable!"

    Word fn -> do
      push $ LOAD fn
      mapM_ compile xs
      push $ CALL (length xs)

  compile (String str) = push $ PUSH (VString str)
  compile (Integer int) = push $ PUSH (VInteger int)
  compile (Double dbl) = push $ PUSH (VDouble dbl)
  compile (Word wrd) = push $ LOAD wrd

  compile _ = return ()

  -- Useful functions

  initProgram :: Program
  initProgram = (0, [(0, [])], [])