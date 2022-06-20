{-# LANGUAGE FlexibleContexts #-}
module Core.Parser.Parser where
  import Core.Parser.AST     (AST(..))
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import qualified Text.Megaparsec.Char.Lexer as L
  import Data.Maybe          (isJust, fromMaybe, fromJust, catMaybes)
  import Data.Void
  {-
    Module: Quark parser
    Description: Lisp like parser using custom combinator library
    Author: thomasvergne
  -}

  -- LISP PARSER

  stringLit :: MonadParsec Void String m => m (Maybe AST)
  stringLit = do
    char '"'
    s <- manyTill L.charLiteral (char '"')
    return . Just $ String s

  integerLit :: MonadParsec Void String m => m (Maybe AST)
  integerLit = do
    n <- some digitChar
    return . Just $ Integer (read n)

  floatLit :: MonadParsec Void String m => m (Maybe AST)
  floatLit = do
    n <- some digitChar
    char '.'
    d <- some digitChar
    return . Just $ Float (read (n ++ "." ++ d))

  blacklist :: String
  blacklist = "() {}[]\n\t\r;\""

  literal :: MonadParsec Void String m => m (Maybe AST)
  literal = stringLit <|> try floatLit <|> integerLit

  identifier :: MonadParsec Void String m => m (Maybe AST)
  identifier = Just . Literal <$> some (noneOf blacklist)

  atom :: MonadParsec Void String m => m (Maybe AST)
  atom = literal <|> identifier

  expr :: MonadParsec Void String m => m (Maybe AST)
  expr = do
    char '('
    x <- many parse'
    char ')'
    Just <$> let xs = catMaybes x
      in case xs of
        (x:xs) -> return $ Node x xs
        [] -> return (Literal "nil")

  begin :: MonadParsec Void String m => m (Maybe AST)
  begin = do
    char '{' >> space
    x <- many parse'
    space >> char '}'
    Just <$> let xs = catMaybes x
      in return $ Node (Literal "begin") xs

  list :: MonadParsec Void String m => m (Maybe AST)
  list = do
    char '[' >> space
    x <- many parse' <* space
    space >> char ']'
    let xs = catMaybes x
      in return . Just $ List xs

  comment :: MonadParsec Void String m => m (Maybe AST)
  comment = do
    char ';' >> manyTill anySingle (char '\n')
    return Nothing

  parse' :: MonadParsec Void String m => m (Maybe AST)
  parse' = ((comment <|> list <|> begin <|> expr <|> atom) <* space) <?> ""

  parseLisp :: String -> Either (ParseErrorBundle String Void) [Maybe AST]
  parseLisp = parse (many parse') ""

  trim :: String -> String
  trim = dropWhile (== ' ')
  
  -- remove eol from string
  format :: String -> String
  format e = unlines $ filter (\z -> not (null z) && (head z /= ';')) (lines e)
  