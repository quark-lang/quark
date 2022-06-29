module Core.Constant.Propagation where
  import Core.Constant.Instances
  import Core.Parser.AST.Expression
  import Data.Maybe (fromMaybe)
  import Prelude hiding (div)
  
  propagate :: Expression -> Expression
  propagate (Node n xs) = do
    let z = Node (propagate n) (map propagate xs)
    case xs of
      [x, y] -> case n of
        Identifier "+" -> fromMaybe z $ propagate x `add` propagate y
        Identifier "-" -> fromMaybe z $ propagate x `sub` propagate y
        Identifier "*" -> fromMaybe z $ propagate x `mul` propagate y
        Identifier "/" -> fromMaybe z $ propagate x `div` propagate y
        _ -> z
      _ -> z
  propagate (List xs) = List $ map propagate xs
  propagate z@(Quoted _) = z
  propagate (Identifier x) = Identifier x
  propagate l@(Literal _) = l