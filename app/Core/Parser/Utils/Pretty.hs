module Core.Parser.Utils.Pretty where
  import System.Console.ANSI
  import Core.Parser.AST           (AST(..))
  import System.Console.ANSI.Codes (ConsoleIntensity(BoldIntensity))
  
  {-
    Module: AST pretty printer
    Description: Pretty prints an AST with indentation and colors
    Author: thomasvergne
  -}

  indent :: Integer -> String
  indent n = replicate (fromIntegral n) ' '

  showAST :: Integer -> AST -> IO ()
  showAST i (Node n xs) = do
    putStr . indent $ i
    setSGR [SetColor Foreground Dull Green]
    putStr "Node "
    setSGR [Reset, SetConsoleIntensity BoldIntensity]
    putStrLn n
    setSGR [Reset]
    mapM_ (showAST (i + 2)) xs
  showAST i val = 
    putStr (indent i) >> case val of
     Integer n -> do
       setSGR [SetColor Foreground Vivid Black]
       putStr "Integer "
       setSGR [Reset, SetColor Foreground Vivid Yellow]
       print n
       setSGR [Reset]
     String s -> do
       setSGR [SetColor Foreground Vivid Black]
       putStr "String "
       setSGR [Reset, SetColor Foreground Dull Green]
       print s
       setSGR [Reset]
     Float x -> do
       setSGR [SetColor Foreground Vivid Black]
       putStr "Float "
       setSGR [Reset, SetColor Foreground Vivid Yellow]
       print x
       setSGR [Reset]
     Literal s -> do
        setSGR [SetColor Foreground Vivid Black]
        putStr "Literal "
        setSGR [Reset, SetConsoleIntensity BoldIntensity]
        putStrLn s
        setSGR [Reset]
     _ -> print "NO CASE"
