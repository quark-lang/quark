{-# LANGUAGE FlexibleContexts #-}
module Core.Parser.Parser where
  import Core.Parser.AST     (AST(..))
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import qualified Text.Megaparsec.Char.Lexer as L
  import Data.Maybe          (isJust, fromMaybe)

  {-
    Module: Quark parser
    Description: Lisp like parser using custom combinator library
    Author: thomasvergne
  -}

  -- LISP PARSER

  stringLit :: MonadParsec String String m => m AST
  stringLit = do
    char '"'
    s <- manyTill L.charLiteral (char '"')
    return $ String s

  integerLit :: MonadParsec String String m => m AST
  integerLit = do
    n <- some digitChar
    return $ Integer (read n)
  
  floatLit :: MonadParsec String String m => m AST
  floatLit = do
    n <- some digitChar
    char '.'
    d <- some digitChar
    return $ Float (read (n ++ "." ++ d))

  blacklist :: String
  blacklist = "() {}[]"

  literal :: MonadParsec String String m => m AST
  literal = stringLit <|> try floatLit <|> integerLit

  identifier :: MonadParsec String String m => m AST
  identifier = Literal <$> some (noneOf blacklist)

  atom :: MonadParsec String String m => m AST
  atom = literal <|> identifier

  expr :: MonadParsec String String m => m AST
  expr = do
    char '('
    x <- many parse'
    char ')'
    case x of
      (x:xs) -> return $ Node x xs
      [] -> return $ Literal "nil"

  begin :: MonadParsec String String m => m AST
  begin = do
    char '{' >> space
    x <- many parse'
    space >> char '}'
    return $ Node (Literal "begin") x
    
  list :: MonadParsec String String m => m AST
  list = do
    char '[' >> space
    x <- many parse' <* space
    space >> char ']'
    return $ List x

  parse' :: MonadParsec String String m => m AST
  parse' = (list <|> begin <|> expr <|> atom) <* space

  parseLisp :: String -> Either (ParseErrorBundle String String) [AST]
  parseLisp = parse (many parse') ""

  trim :: String -> String
  trim = dropWhile (== ' ')

  -- remove eol from string
  format :: String -> String
  format e = concat $ filter (\z -> not (null z) && (head z /= ';')) (lines e)
  