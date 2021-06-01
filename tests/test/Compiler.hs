module Test.Compiler where
  import Test.Hspec
  import Control.Exception (evaluate)
  import Core.Compiler
  import Core.Parser
  import Control.Monad.State (runState)

  compile' :: Atom -> [Page]
  compile' x = let (_,res) = runState (compile x) initProgram in let (_,p,_) = res in p

  compilerTest :: IO()
  compilerTest = do
    putStrLn "\nCompiler tests..."
    hspec $ do
      describe "Values" $ do
        it "should push integer" $ do
          compile' (Integer 7) `shouldBe` [[ PUSH (VInteger 7) ]]

        it "should push double" $ do
          compile' (Double 7.5) `shouldBe` [[ PUSH (VDouble 7.5) ]]

        it "should push string" $ do
          compile' (String "test") `shouldBe` [[ PUSH (VString "test") ]]

      describe "Variable declaration" $ do
        it "should fail if name isn't a word" $ do
          evaluate  (compile' $ Expression [ Word "let", String "x", Integer 5 ]) `shouldThrow` anyException

        it "should store content" $ do
          compile' (Expression [ Word "let", Word "x", Integer 5 ]) `shouldBe` [[ PUSH (VInteger 5), STORE "x" ]]

      describe "Functions" $ do
        it "should fail if arguments aren't an expression" $ do
          evaluate (compile' $ Expression [ Word "fn", Word "t", Expression [] ]) `shouldThrow` anyException

        it "should fail if argument is not a word" $ do
          evaluate (compile' $ Expression [ Word "fn", Expression [String "x"], Expression [] ]) `shouldThrow` anyException

        it "should fail if no body" $ do
          evaluate (compile' $ Expression [ Word "fn", Expression [] ]) `shouldThrow` anyException

        it "should compile and push new page" $ do
          compile' (Expression [ Word "fn", Expression [], Expression [] ]) `shouldBe` [[ LOAD_SEGMENT 1 ], [  ]]

        it "could be stored as a variable" $ do
          compile' (Expression [Word "let", Word "fun", Expression [ Word "fn", Expression [], Expression [] ]])
          `shouldBe`
          [[ LOAD_SEGMENT 1, STORE "fun" ], [  ]]

        it "should handle functions inside functions" $ do
          compile' (Expression [
              Word "fn", Expression [], Expression [
                Word "fn", Expression [], Expression []
              ]
            ])
          `shouldBe` [[LOAD_SEGMENT 1], [LOAD_SEGMENT 2], []]
