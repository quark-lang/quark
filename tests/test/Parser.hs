module Test.Parser where
  import Test.Hspec
  import Control.Exception (evaluate)
  import Core.Parser 
  import Test.Hspec.Parsec hiding (parse)

  parserTest = hspec $ do
    describe "Brackets" $ do
      it "should error on mismatched brackets" $ do
        parse `shouldFailOn` "(()"
      
    describe "Numbers" $ do
      it "should parse integers" $ do
        parse "5" `shouldParse` Integer 5
        parse "(t 5)" `shouldParse` Expression [ Word "t", Integer 5 ]
      
      it "should parse doubles" $ do
        parse "5.0" `shouldParse` Double 5.0

      it "should fail on space in double" $ do
        parse `shouldFailOn` "(5 .0)"
        parse `shouldFailOn` "(5 . 0)"

    describe "Strings" $ do
      it "should fail on unclosed quote" $ do
        parse `shouldFailOn` "\"test"
      
      it "should parse escaped characters" $ do
        parse "\"test\\\" bruh\"" `shouldParse` String "test\\\" bruh"
        parse "\"test\\n bruh\"" `shouldParse` String "test\\n bruh"
        parse "\"test\\r bruh\"" `shouldParse` String "test\\r bruh"
        parse "\"test\\t bruh\"" `shouldParse` String "test\\t bruh"

      it "should parse integers in string" $ do
        parse "\"test 5\"" `shouldParse` String "test 5"
  
    describe "Examples" $ do
      it ("should correctly parse " ++ "\"(print \"test\" (+ 5 2.5))\"") $ do
        parse "(print \"test\" (+ 5 2.5))" `shouldParse` Expression [ Word "print", String "test", Expression [ Word "+", Integer 5, Double 2.5 ] ]

      it ("should correctly parse " ++ "\"(+ (+ 5 2) (- 1 (print 4)))\"") $ do
        parse "(+ (+ 5.0 2) (- 1 (print 4)))" 
        `shouldParse` 
        Expression [ Word "+", Expression [ Word "+", Double 5.0, Integer 2 ], Expression [ Word "-", Integer 1, Expression [ Word "print", Integer 4 ] ] ]