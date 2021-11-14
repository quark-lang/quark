{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Core.Parser.Combinator where
  import Data.Bifunctor         (first, second)
  import Control.Monad.Trans    (MonadTrans(lift), MonadIO(liftIO))
  import Control.Monad          (MonadPlus(mzero, mplus))
  import Control.Applicative    (Alternative((<|>), empty))
  import Data.Foldable          (asum)
  import Control.Monad.Identity (MonadPlus(..), Identity(runIdentity))
  import Data.Char              (isLetter, isDigit)
  
  {-
    Module: Parser combinator
    Description: Parser custom library for Quark
    Author: thomasvergne
  -}

  -- s is the type of data which is being treated (e.g. a String input stream)
  -- a is the type of data which is being parsed
  -- m is the type of the monad
  -- e is the type of error
  newtype ParserT s e m a = ParserT { runParserT :: s -> m (Either [e] a, s) }
  type Parser s a = ParserT s String Identity a

  runParser :: Parser s a -> s -> (Either [String] a, s)
  runParser p s = runIdentity $ runParserT p s

  -- Stream class describe how can a type s be destructured 
  class Eq s => Stream s where
    type Token s :: *
    pop :: s -> Maybe (s, Token s)
    elem :: Token s -> s -> Bool

  -- Stream instance for all generic lists
  instance Eq a => Stream [a] where
    type Token [a] = a
    pop [] = Nothing
    pop (x:xs) = Just (xs, x)

    elem x xs = Prelude.elem x xs

  instance Functor f => Functor (ParserT s e f) where
    fmap f (ParserT p) = ParserT $ fmap (first (second f)) . p

  instance Monad m => Applicative (ParserT s e m) where
    pure a = ParserT $ pure . (Right a,)
    ParserT f <*> ParserT a = ParserT \s -> do
      (f', s') <- f s
      (a', s'') <- a s'
      return (f' <*> a', s'')

  instance Monad m => Monad (ParserT s e m) where
    return = pure
    ParserT p >>= f = ParserT \s -> do
      (a, s') <- p s
      case a of
        Right x -> runParserT (f x) s'
        Left es -> pure (Left es, s')

  instance MonadTrans (ParserT s e) where
    lift m = ParserT \s -> do
      a <- m
      pure (Right a, s)

  instance MonadIO m => MonadIO (ParserT s e m) where
    liftIO = lift . liftIO

  instance Monad m => Alternative (ParserT s e m) where
    empty = ParserT \s -> pure (Left [], s)
    ParserT a <|> ParserT b = ParserT \s -> do
      x <- a s
      case x of
        (Right _,_) -> pure x
        (Left _,_) -> b s

  instance MonadPlus m => MonadPlus (ParserT s e m) where
    mzero = ParserT $ const mzero
    mplus (ParserT p) (ParserT q) = ParserT \s -> mplus (p s) (q s)

  instance MonadPlus m => MonadFail (ParserT s e m) where
    fail _ = ParserT $ const empty

  -- consume if predicate is satisfied
  satisfy ::
    (Applicative m, Stream s) =>
      (Token s -> Bool) ->
      ParserT s e m (Token s)
  satisfy p = ParserT \s -> case pop s of
    Just (s', t) | p t -> pure (Right t, s')
    _ -> pure (Left [], s)

  char :: Applicative m => Token String -> ParserT String e m (Token String)
  char c = satisfy (== c)

  string :: Monad m => [Token String] -> ParserT String e m [Token String]
  string [] = pure []
  string (c:cs) = char c >> string cs >> pure (c:cs)

  try :: Monad m => ParserT s e m a -> ParserT s e m a
  try p = ParserT \s -> do
    (a, s') <- runParserT p s
    case a of
      Left _ -> pure (Left [], s)
      Right x -> pure (Right x, s')

  -- consume if parsing passes or return Nothing
  optional :: Monad m => ParserT s e m a -> ParserT s e m (Maybe a)
  optional p = (Just <$> p) <|> pure Nothing

  throw :: Monad m => e -> ParserT s e m a
  throw e = ParserT \s -> pure (Left [e], s)

  -- parse zero or more times (it does not fail)
  many :: Monad m => ParserT s e m a -> ParserT s e m [a]
  many p = ParserT \s -> do
    r <- runParserT p s
    case r of
      (Right x, s') -> do
        (xs, s'') <- runParserT (many p) s'
        case xs of
          Right xs' -> pure (Right (x:xs'), s'')
          Left _ -> pure (Right [x], s')
      (Left _, s') -> pure (Right [], s')

  -- parse one or more times
  many1 :: Monad m => ParserT s e m a -> ParserT s e m [a]
  many1 p = (:) <$> p <*> many p
  
  between :: Monad m => ParserT s e m a -> ParserT s e m b -> ParserT s e m c -> ParserT s e m c
  between open close p = open *> p <* close

  -- parse zero or more times, separated by sep
  sepBy :: Monad m => ParserT s e m a -> ParserT s e m b -> ParserT s e m [a]
  sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

  -- parse one token if it's in the stream given
  oneOf :: (Stream s, Applicative m) => s -> ParserT s e m (Token s)
  oneOf s = satisfy (`Core.Parser.Combinator.elem` s)
  
  noneOf :: (Stream s, Applicative m) => s -> ParserT s e m (Token s)
  noneOf s = satisfy (not . (`Core.Parser.Combinator.elem` s))
  
  anyChar :: (Stream s, Applicative m) => ParserT s e m (Token s)
  anyChar = satisfy (const True)

  -- allow providing a list of parsers to try
  choices :: Monad m => [ParserT s e m a] -> ParserT s e m a
  choices = asum

  {- Implementation of basic useful functions -}

  letter :: Applicative m => ParserT String e m Char
  letter = satisfy isLetter

  digit :: Applicative m => ParserT String e m Char
  digit = satisfy isDigit

  space :: Applicative m => ParserT String e m Char
  space = oneOf " "

  spaces :: Monad m => ParserT String e m String
  spaces = many space