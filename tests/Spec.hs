module Main where
  import Test.Parser
  import Test.Compiler

  main = parserTest >> compilerTest 