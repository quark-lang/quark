module Test.Parser where
  import Test.Hspec
  import Control.Exception (evaluate)
  import Core.Parser 
  import Test.Hspec.Parsec hiding (parse)

  main :: IO()
  main = hspec $ do
    describe "Brackets" $ do
      it "should error on mismatched brackets" $ do
        parse `shouldFailOn` "(()"
      
    describe "Numbers" $ do
      it "should parse integer" $ do
        parse "5" `shouldParse` Integer 5
        parse "(t 5)" `shouldParse` Expression [ Word "t", Integer 5 ]
        parse "-5" `shouldParse` Integer (-5)
      
      it "should parse double" $ do
        parse "5.0" `shouldParse` Double 5.0
        parse "-5.12" `shouldParse` Double (-5.12)

      it "should fail on space in double" $ do
        parse `shouldFailOn` "(5 .0)"
        parse `shouldFailOn` "(5 . 0)"