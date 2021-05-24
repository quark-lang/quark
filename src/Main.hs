module Main where
  import Core.Parser (parse)
  
  main = do
    print $ parse "(print 5.2)"