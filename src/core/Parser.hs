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

  escape :: Parser String
  escape = do
      d <- char '\\'
      c <- oneOf "\\\"0nrvtbf"
      return [d, c]

  nonEscape :: Parser Char
  nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

  character :: Parser String
  character = fmap return nonEscape <|> escape

  string :: Parser Atom
  string = do
    char '"'
    x <- many character
    char '"'
    pure $ String $ concat x

  double :: Parser Atom
  double = do
    num <- many1 digit
    char '.'
    mantissa <- many1 digit
    pure $ Double . read $  num <> "." <> mantissa

  integer :: Parser Atom
  integer = do
    num <- many1 digit
    pure $ Integer . read $ num

  expression :: Parser Atom
  expression = Expression <$> sepBy parse' spaces

  parse' :: Parser Atom
  parse' = word
       <|> try double
       <|> integer
       <|> string
       <|> do
            char '('
            x <- try expression
            char ')'
            pure x
  parse = runParser parse' () ""