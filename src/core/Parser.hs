{-# LANGUAGE BlockArguments #-}
module Core.Parser where
  import Text.ParserCombinators.Parsec hiding (string)

  symbol :: Parser Char
  symbol = oneOf "!&|~#*+$%-@^<=_/:>?"

  data Atom
    = Word String
    | Expression [Atom]
    | Integer Integer
    | Double Double
    | String String
    deriving (Read, Show, Eq)

  word :: Parser Atom
  word = do
    first_char <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first_char : rest
    pure $ Word atom

  string :: Parser Atom
  string = do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    pure $ String x

  double :: Parser Atom
  double = do
    b <- negative
    num <- many1 digit
    char '.'
    mantissa <- many1 digit
    pure $ Double . read $ b <> num <> "." <> mantissa

  negative :: Parser String
  negative = do
    x <- optionMaybe $ char '-'
    pure $ case x of
      Just _ -> "-"
      Nothing -> ""
            
  integer :: Parser Atom
  integer = do
    b <- negative
    num <- many1 digit
    pure $ Integer . read $ b <> num

  expression :: Parser Atom
  expression = Expression <$> sepBy parse' spaces

  parse' :: Parser Atom
  parse' = try double
       <|> integer
       <|> string
       <|> word
       <|> do
            char '('
            x <- try expression
            char ')'
            pure x
  parse = runParser parse' () ""