{-# LANGUAGE TypeApplications #-}
module Core.Parser.Parser where
  import Core.Parser.Combinator
  import Core.Parser.AST
  import Control.Applicative ((<|>))
  import Data.Maybe (isJust)
  
  {-
    Module: Quark parser
    Description: Lisp like parser using custom combinator library
    Author: thomasvergne
  -}

  -- Parse an expression
  parseExpr :: Parser String AST
  parseExpr = do
    char '('
    name <- lexeme parseWord
    args <- many parse
    char ')'
    return $ Node name args

  parseWord :: Parser String String
  parseWord = many1 $ letter <|> digit <|> noneOf "() {}[]"

  parseString :: Parser String AST
  parseString = String <$> (char '"' *> many (noneOf "\"") <* char '"')
  
  parseBeginSugar :: Parser String AST
  parseBeginSugar = do
    lexeme $ char '{'
    expr <- many parse
    char '}'
    return $ Node "begin" expr

  parseListSugar :: Parser String AST
  parseListSugar = do
    lexeme $ char '['
    expr <- many parse
    char ']'
    return $ Node "list" expr

  parseNumber :: Parser String AST
  parseNumber = do
    sign <- optional $ char '-'
    num <- many1 digit
    return . Integer . read $ parseSign sign ++ num
    where parseSign s = if isJust s then let (Just x) = s in "-" else ""

  getInteger :: AST -> Integer
  getInteger (Integer i) = i
  getInteger _ = error "Not an integer"

  parseFloat :: Parser String AST
  parseFloat = do
    num <- show . getInteger <$> parseNumber
    char '.'
    dec <- many1 digit
    return $ Float (read $ num ++ "." ++ dec)

  parseSugar :: Parser String AST
  parseSugar = choices
    [
      parseBeginSugar,
      parseListSugar
    ]

  parse :: Parser String AST
  parse = lexeme . choices $
    [
      parseSugar,
      parseExpr,
      parseString,
      parseFloat,
      parseNumber,
      Literal <$> parseWord
    ]

  lexeme p = p <* spaces

  -- remove eol from string
  format :: String -> String
  format = concat . lines
  