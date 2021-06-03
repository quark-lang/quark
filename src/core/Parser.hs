{-# LANGUAGE BlockArguments #-}
module Core.Parser where
  import Text.ParserCombinators.Parsec hiding (parse)
  import qualified Text.Parsec
  import qualified Text.Parsec.Token as P
  import Text.Parsec.Language

  data Atom
    = Word String
    | Expression [Atom]
    | Integer Integer
    | Double Double
    | String String
    deriving (Read, Show, Eq)

  lexer = P.makeTokenParser def

  -- Desugaring { instr ... } into (begin instr ...)
  parseBlock = do
    instr <- P.braces lexer (many $ parseAtom <|> parseList)
    pure . Expression $ Word "begin" : instr

  -- Desugaring [i1 i2 ...] into (list i1 i2 ...)
  parseListBlock = do
    instr <- P.brackets lexer (many $ parseAtom <|> parseList)
    pure . Expression $ Word "list" : instr

  -- Main parsing entry
  parseList = Expression <$> 
        P.parens lexer (many $ parseAtom <|> parseList) 
    <|> parseBlock
    <|> parseListBlock

  -- Parsing elements in expression
  parseAtom = choice 
    [ Word <$> (P.identifier lexer <|> P.operator lexer)
    , try $ Double <$> P.float lexer
    , Integer <$> P.integer lexer
    , String <$> P.stringLiteral lexer
    ]

  -- Lisp parser definition
  def = emptyDef 
    {
        P.commentStart   = "/*"
      , P.commentEnd     = "*/"
      , P.commentLine    = "#"
      , P.nestedComments = True
      , P.identStart     = letter <|> char '_'
      , P.identLetter    = alphaNum <|> oneOf "_'"
      , P.opStart        = P.opLetter emptyDef
      , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , P.reservedOpNames= []
      , P.reservedNames  = ["import"]
      , P.caseSensitive  = True
    }
  parse = runParser parseList () ""