module Core.Constant.Instances where
  import Prelude hiding (div)
  import Core.Parser.AST
  import Control.Monad (zipWithM)
  class Propagation a where
    add :: a -> a -> Maybe a
    mul :: a -> a -> Maybe a
    sub :: a -> a -> Maybe a
    div :: a -> a -> Maybe a

  instance Propagation Literal where
    Integer a `add` Integer b = Just $ Integer (a + b)
    Float a `add` Float b = Just $ Float (a + b)
    String a `add` String b = Just $ String (a ++ b)
    _ `add` _ = Nothing
    
    Integer a `mul` Integer b = Just $ Integer (a * b)
    Float a `mul` Float b = Just $ Float (a * b)
    String a `mul` String b = Nothing
    _ `mul` _ = Nothing

    Integer a `sub` Integer b = Just $ Integer (a - b)
    Float a `sub` Float b = Just $ Float (a - b)
    String a `sub` String b = Nothing
    _ `sub` _ = Nothing

    Integer a `div` Integer b = Just $ Float (fromIntegral a / fromIntegral b)
    Float a `div` Float b = Just $ Float (a / b)
    String a `div` String b = Nothing
    _ `div` _ = Nothing
  
  instance Propagation Expression where
    Literal a `add` Literal b = Literal <$> (a `add` b)
    Node n xs `add` Node n' xs' = Node <$> (n `add` n') <*> zipWithM add xs xs'
    List xs `add` List ys = List <$> zipWithM add xs ys
    _ `add` _ = Nothing

    Literal a `mul` Literal b = Literal <$> (a `mul` b)
    Node n xs `mul` Node n' xs' = Node <$> (n `mul` n') <*> zipWithM mul xs xs'
    List xs `mul` List ys = List <$> zipWithM mul xs ys
    _ `mul` _ = Nothing

    Literal a `sub` Literal b = Literal <$> (a `sub` b)
    Node n xs `sub` Node n' xs' = Node <$> (n `sub` n') <*> zipWithM sub xs xs'
    List xs `sub` List ys = List <$> zipWithM sub xs ys
    _ `sub` _ = Nothing

    Literal a `div` Literal b = Literal <$> (a `div` b)
    Node n xs `div` Node n' xs' = Node <$> (n `div` n') <*> zipWithM div xs xs'
    List xs `div` List ys = List <$> zipWithM div xs ys
    _ `div` _ = Nothing