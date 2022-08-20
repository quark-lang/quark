{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Core.Parser.Parser where
  import Core.Parser.AST
  import Text.Megaparsec
  import Text.Megaparsec.Char (char, digitChar, space, space1, alphaNumChar)
  import Data.Maybe           (isJust, fromMaybe, fromJust, catMaybes)
  import Data.Void
  import Control.Monad        (void)
  import Core.Utility.Sugar   (buildBeginSugar, eliminateSugar)
  import Debug.Trace          (traceShow)
  import Data.Functor
  import qualified Text.Megaparsec.Char.Lexer as L
  
  {-
    Module: Quark parser
    Description: Lisp like parser using custom combinator library
    Author: thomasvergne
  -}

  -- LISP PARSER

  sc :: MonadParsec Void String m => m ()
  sc = L.space space1 lineCmnt empty
    where
      lineCmnt  = L.skipLineComment ";"

  lexeme :: MonadParsec Void String m => m a -> m a
  lexeme = L.lexeme sc

  stringLit :: MonadParsec Void String m => m (Maybe Literal)
  stringLit = do
    char '"'
    s <- manyTill L.charLiteral (char '"')
    return . Just $ String s

  integerLit :: MonadParsec Void String m => m (Maybe Literal)
  integerLit = do
    n <- some digitChar
    return . Just $ Integer (read n)
  
  charLit :: MonadParsec Void String m => m (Maybe Literal)
  charLit = do
    char '\''
    c <- L.charLiteral
    char '\''
    return . Just $ Char c

  floatLit :: MonadParsec Void String m => m (Maybe Literal)
  floatLit = do
    n <- some digitChar
    char '.'
    d <- some digitChar
    return . Just $ Float (read (n ++ "." ++ d))

  blacklist :: String
  blacklist = "() {}[];\r\n\t\"@"

  literal :: MonadParsec Void String m => m (Maybe Literal)
  literal = charLit <|> stringLit <|> try floatLit <|> integerLit

  identifier :: MonadParsec Void String m => m (Maybe Expression)
  identifier = Just . Identifier <$> some (noneOf blacklist)

  atom :: MonadParsec Void String m => m (Maybe Expression)
  atom = literal >>= \case
    Just x -> return . Just . Literal $ x
    Nothing -> return Nothing

  expr :: MonadParsec Void String m => m (Maybe Expression)
  expr = do
    char '('
    x <- many parse'
    char ')'
    Just <$> let xs = catMaybes x
      in case xs of
        (x:xs) -> return $ Node x xs
        [] -> return (Identifier "nil")

  begin :: MonadParsec Void String m => m (Maybe Expression)
  begin = do
    char '{' >> space
    x <- many parse'
    space >> char '}'
    Just <$> let xs = catMaybes x
      in return (Node (Identifier "begin") xs)

  list :: MonadParsec Void String m => m (Maybe Expression)
  list = do
    char '[' >> space
    x <- many parse' <* space
    space >> char ']'
    let xs = catMaybes x
      in return . Just $ List xs

  quoted :: MonadParsec Void String m => m (Maybe Expression)
  quoted = do
    char '@'
    x <- parse'
    return $ Quoted <$> x

  comment :: MonadParsec Void String m => m (Maybe Expression)
  comment = do
    char ';' >> manyTill anySingle (void (char '\n') <|> eof)
    return Nothing

  parse' :: MonadParsec Void String m => m (Maybe Expression)
  parse' = lexeme (list <|> begin <|> expr <|> atom <|> quoted <|> identifier) <?> ""

  parseLisp :: String -> Either (ParseErrorBundle String Void) [Expression]
  parseLisp = parse (catMaybes <$> many (space *> parse' <* space)) ""

  