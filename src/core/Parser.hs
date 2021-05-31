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
    num <- many1 digit
    char '.'
    mantissa <- many1 digit
    pure $ Double . read $ num <> "." <> mantissa

  integer :: Parser Atom
  integer = Integer . read <$> many1 digit

  expression :: Parser Atom
  expression = Expression <$> sepBy parse' spaces

  parse' :: Parser Atom
  parse' = word
       <|> string
       <|> try double 
       <|> integer
       <|> do
            char '('
            x <- try expression
            char ')'
            pure x
  parse = runParser parse' () ""