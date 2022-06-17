module Core.Compiler.Uncurry where
  import qualified Data.Map as M
  import qualified Core.Inference.Type.AST as A
  import Core.Inference.Type.Pretty
  import Control.Monad.State
  import Prelude hiding (uncurry)

  type Argument = (String, Type)
  type Functions = M.Map String Int -- Name -> Number of arguments
  type MonadCurry m = (Monad m, MonadState Functions m, MonadIO m)

  addFunction :: MonadCurry m => String -> Int -> m ()
  addFunction name n = modify $  M.insert name n

  data Type
    = TVar Int | TId String
    | [Type] :-> Type
    | Int | String | Float | Bool | Any
    | TApp Type [Type]
    | ListT Type
    deriving (Eq, Ord, Show)

  data UncurriedPattern
    = VarP String Type
    | LitP A.Literal Type
    | WilP Type
    | AppP Argument [UncurriedPattern] [Type]
    deriving (Show, Eq)

  data UncurriedAST
    = AppE Argument [UncurriedAST] Type
    | AbsE [Argument] UncurriedAST
    | VarE String Type
    | LetInE Argument UncurriedAST UncurriedAST
    | ListE [UncurriedAST] Type
    | LetE Argument UncurriedAST
    | LitE A.Literal Type
    | IfE UncurriedAST UncurriedAST UncurriedAST
    -- (Name, [Generics])
    | DataE (String, [Type]) [(String, Type)]
    -- Pattern | [(Case, AST)]
    | PatternE UncurriedAST [(UncurriedPattern, UncurriedAST)]
    deriving (Eq, Show)

  getReturn :: A.Type -> A.Type
  getReturn (_ A.:-> t) = getReturn t
  getReturn t = t

  uncurryType :: A.Type -> Type
  uncurryType (A.TVar i) = TVar i
  uncurryType (A.TId s) = TId s
  uncurryType z@(A.TApp _ _)
    = let (ts, Just t) = uncurryTApp z
        in TApp t ts
  uncurryType z@(t1 A.:-> t2)
    = let (ts, Just n) = uncurryTArrow z
        in ts :-> n
  uncurryType (A.ListT t) = ListT (uncurryType t)
  uncurryType (A.Int) = Int
  uncurryType (A.String) = String
  uncurryType (A.Float) = Float
  uncurryType (A.Bool) = Bool
  uncurryType (A.Any) = Any

  uncurryTApp :: A.Type -> ([Type], Maybe Type)
  uncurryTApp (A.TApp n t) =
    let (ts, m) = uncurryTApp n
      in (uncurryType t : ts, m)
  uncurryTApp t = ([], Just (uncurryType t))

  uncurryTArrow :: A.Type -> ([Type], Maybe Type)
  uncurryTArrow (t1 A.:-> t2) =
    let (ts, mb) = uncurryTArrow t2
      in (uncurryType t1 : ts, mb)
  uncurryTArrow t = ([], Just $ uncurryType t)

  runUncurry :: (Monad m, MonadIO m) => A.TypedAST -> Functions -> m (UncurriedAST, Functions)
  runUncurry a f = runStateT (uncurry a) f

  uncurry :: MonadCurry m => A.TypedAST -> m UncurriedAST
  uncurry z@(A.AppE _ _ t) = do
    let (Just a@(n, t), args, t') = uncurryApp z
    args' <- mapM uncurry args
    get >>= \e -> case M.lookup n e of
      Just n' -> do
        if n' /= length args' then do
          let missingArgs = [0..n' - length args' - 1]
          let app  = AppE
                      (n, uncurryType t)
                      (args' ++ map (`VarE` Any) ["_" ++ show i | i <- missingArgs])
                      (uncurryType t)
          return $ foldl (\acc x -> AbsE [(x, Any)] acc) (AbsE [("_0", Any)] app) ["_" ++ show i | i <- [1..n' - length args' - 1]]
        else return $ AppE (n, uncurryType t) args' (uncurryType t)
      Nothing -> return $ AppE (n, uncurryType t) args' (uncurryType t)
  uncurry (A.LetE (n, t') z@(A.AbsE _ _)) = do
    let (args, b') = uncurryAbs z
    addFunction n (length args)
    LetE (n, uncurryType t') . AbsE args <$> uncurry b'
  uncurry (A.LetE (n, t') t) = LetE (n, uncurryType t') <$> (uncurry t)
  uncurry (A.LetInE (n, t') v b) = LetInE (n, uncurryType t') <$> (uncurry v) <*> (uncurry b)
  uncurry z@(A.AbsE _ _) =
    let (args, t') = uncurryAbs z
      in AbsE args <$> (uncurry t')
  uncurry (A.IfE c t e) = IfE <$> (uncurry c) <*> (uncurry t) <*> (uncurry e)
  uncurry (A.ListE e t) = ListE <$> mapM uncurry e <*> pure (uncurryType t)
  uncurry (A.LitE l t) = return $ LitE l (uncurryType t)
  uncurry (A.PatternE p t) = do
    p' <- uncurry p
    t' <- mapM (uncurry . snd) t
    return $ PatternE p' (zip (map (uncurryPattern . fst) t) t')
  uncurry (A.VarE n t) = return $ VarE n (uncurryType t)
  uncurry (A.DataE (n, t) b)
    = let t' = map uncurryType t
          b' = map (uncurryType . snd) b
          n' = map fst b
      in return $ DataE (n, t') (zip n' b')

  uncurryAbs :: A.TypedAST -> ([Argument], A.TypedAST)
  uncurryAbs (A.AbsE (n1, t1) (A.AbsE (n2, t2) t)) =
    let (args, t') = uncurryAbs t
      in ((n1, uncurryType t1) : (n2, uncurryType t2) : args, t')
  uncurryAbs (A.AbsE (n, t') t) = ([(n, uncurryType t')], t)
  uncurryAbs x = ([], x)

  uncurryPattern :: A.TypedPattern -> UncurriedPattern
  uncurryPattern (A.VarP n t) = VarP n $ uncurryType t
  uncurryPattern (A.LitP l t) = LitP l $ uncurryType t
  uncurryPattern (A.WilP t) = WilP $ uncurryType t
  uncurryPattern z@(A.AppP n ps t) =
    let (Just (n, t), args, t') = uncurryAppP z
      in AppP (n, uncurryType t) (map uncurryPattern args) (map uncurryType t')

  uncurryAppP :: A.TypedPattern -> (Maybe (String, A.Type), [A.TypedPattern], [A.Type])
  uncurryAppP (A.AppP name arg _) =
    let (name', args', t') = uncurryAppP name
      in (name', args' ++ [arg], t' ++ [A.getTypeP arg])
  uncurryAppP (A.VarP name t) = (Just (name, t), [], [])
  uncurryAppP t = (Nothing, [t], [A.getTypeP t])

  uncurryApp :: A.TypedAST -> (Maybe (String, A.Type), [A.TypedAST], [A.Type])
  uncurryApp (A.AppE name arg _) =
    let (name', args', t') = uncurryApp name
      in (name', args' ++ [arg], t' ++ [A.getType arg])
  uncurryApp (A.VarE name t) = (Just (name, t), [], [])
  uncurryApp t = (Nothing, [t], [A.getType t])