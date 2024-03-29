{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Core.Parser.Parser where
  import Core.Parser.AST
  import Text.Megaparsec
  import Text.Megaparsec.Char (char, digitChar, space, space1, alphaNumChar, eol)
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

  stringLit :: MonadParsec Void String m => m Literal
  stringLit = do
    char '"'
    s <- manyTill L.charLiteral (char '"')
    return $ String s

  integerLit :: MonadParsec Void String m => m Literal
  integerLit = do
    n <- some digitChar
    return $ Integer (read n)

  charLit :: MonadParsec Void String m => m Literal
  charLit = do
    char '\''
    c <- L.charLiteral
    char '\''
    return $ Char c

  floatLit :: MonadParsec Void String m => m Literal
  floatLit = do
    n <- some digitChar
    char '.'
    d <- some digitChar
    return $ Float (read (n ++ "." ++ d))

  blacklist :: String
  blacklist = "() {}[];\r\n\t\"@"

  literal :: MonadParsec Void String m => m Literal
  literal = charLit <|> stringLit <|> try floatLit <|> integerLit

  identifier :: MonadParsec Void String m => m Expression
  identifier = Identifier <$> some (noneOf blacklist)

  atom :: MonadParsec Void String m => m Expression
  atom = literal <&> Literal

  expr :: MonadParsec Void String m => m Expression
  expr = do
    char '('
    x <- many parse'
    char ')'
    case x of
      x:xs -> return $ Node x xs
      [] -> return $ Identifier "Void"

  begin :: MonadParsec Void String m => m Expression
  begin = do
    char '{' >> space
    x <- many parse'
    space >> char '}'
    return (Node (Identifier "begin") x)

  list :: MonadParsec Void String m => m Expression
  list = do
    char '[' >> space
    x <- many parse' <* space
    space >> char ']'
    return $ List x

  quoted :: MonadParsec Void String m => m Expression
  quoted = do
    char '@'
    Quoted <$> parse'


  parse' :: MonadParsec Void String m => m Expression
  parse' = lexeme (list <|> begin <|> expr <|> atom <|> quoted <|> identifier) <?> ""

  parseLisp :: String -> Either (ParseErrorBundle String Void) [Expression]
  parseLisp = parse (many (sc *> parse' <* sc)) ""

  