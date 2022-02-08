module Interpreter (
  eval,
  builtin,
) where

import Types
import Util

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

import Data.IORef


eval :: Expr -> Context Expr
eval = \case
  -- Trivial stuff
  ENil      -> pure ENil
  EInt n    -> pure $ EInt n
  EBool b   -> pure $ EBool b
  EList []  -> pure $ EList []
  ESymbol s -> do
    getEnv <&> (Map.lookup s . unEnv) >>= \case
      Nothing -> error $ "symbol not found: " <> T.unpack s
      Just s' -> pure s'

  -- Control structures
  EList [ESymbol "if", cond, then', else'] ->
    ifM (truthy cond) (eval then') (eval else')

  EList (ESymbol "cond" : clauses) -> do
    let evalCondition :: Expr -> Context (Maybe Expr)
        evalCondition = \case
          EList [i, t] -> ifM (truthy i) (Just <$> eval t) (pure Nothing)
          _            -> error "cond's arguments should be lists of two elements"
        genCond :: [Expr] -> Context Expr
        genCond []       = pure ENil
        genCond (x : xs) = evalCondition x >>= maybe (genCond xs) pure
    genCond clauses

  EList [ESymbol "let", EList bindings, body] -> do
    env <- getEnv
    let evalBinding e = \case
          EList [ESymbol name, bindTo] -> (name, ) <$> locally e (eval bindTo)
          _                            -> error "evalBinding"
        evalBindings e = \case
          []       -> pure e
          (x : xs) -> evalBinding e x >>= \b -> evalBindings (insert b e) xs
    binds <- evalBindings env bindings
    locally (binds <> env) (eval body)

  -- Functions and applications
  EList [ESymbol "define", ESymbol name, EList params, body] -> do
    env <- getEnv
    let addFn = (name, fun)                      -- tying a tiny knot <3
        fun   = EFun \args ->
          locally (insert addFn (asEnv params args <> env)) (eval body)
    modifyContext (insert addFn)
    pure fun                                     -- oh yeah

  EList [ESymbol "lambda", EList params, body] -> do
    freeVars <- getEnv
    pure $ ELambda freeVars \args ->
      locally (asEnv params args <> freeVars) (eval body)

  EList (f : xs) -> eval f >>= \case
     EFun        g -> g =<< traverse eval xs
     ELambda env g -> locally env (g =<< traverse eval xs)
     _             -> error $ "not a function: " <> show f

  _ -> error "eval"

builtin :: IO (IORef Env)
builtin = newIORef $ fromList
  [ -- arithmetic
    ("+"   , EFun $ nAryOp foldl' 0 (+))
  , ("-"   , EFun $ nAryOp foldr  0 (-))
  , ("*"   , EFun $ nAryOp foldl' 1 (*))
  , ("/"   , EFun $ nAryOp foldr  1 div)
    -- comparisons
  , ("^"   , EFun $ nAryOp foldr  1 (^)    )
  , ("<"   , EFun $ fmap EBool         . lt)
  , (">"   , EFun $ fmap (EBool . not) . lt)
  , ("="   , EFun $ foldM1 eq              )
    -- list primitives
  , ("list", EFun $ pure . EList)
  , ("car" , EFun car           )
  , ("cdr" , EFun cdr           )
  , ("cons", EFun consList      )
  ]

nAryOp :: Applicative f => (a -> b -> [Int] -> Int) -> b -> a -> [Expr] -> f Expr
nAryOp cata def op xs
  | length xs == length allNumbers = pure . EInt . cata op def $ allNumbers
  | otherwise                      = error "nAryOp"
 where
  allNumbers = mapMaybe unNumber xs

  unNumber :: Expr -> Maybe Int
  unNumber = \case
    EInt n -> Just n
    _      -> Nothing

eq :: Expr -> Expr -> Context Expr
eq (EInt x)    (EInt y)    = pure . EBool $ x == y
eq (ESymbol x) (ESymbol y) = pure . EBool $ x == y
eq (EBool x)   (EBool y)   = pure . EBool $ x == y
eq ENil        ENil        = pure . EBool $ True
eq (EList xs)  (EList ys)  = do
  res <- zipWithM eq xs ys
  EBool . and <$> traverse (fmap (True ==) . truthy) res
eq _ _ = pure $ EBool False

lt :: [Expr] -> Context Bool
lt = go
 where
  go = \case
    (EInt x : EInt y : xs) -> ((x < y) &&) <$> go xs
    [_]                    -> pure True
    []                     -> pure True
    e                      -> error $ "lt: " <> show e

car :: [Expr] -> Context Expr
car = \case
  [EList (x : _)] -> pure x
  [EList{}]       -> pure ENil
  e               -> error $ "car:" <> show e

cdr :: [Expr] -> Context Expr
cdr = \case
  [EList (_ : xs)] -> pure (EList xs)
  [EList {}]       -> pure ENil
  e                -> error $ "cdr: " <> show e

consList :: [Expr] -> Context Expr
consList = \case
  [a, EList xs] -> pure $ EList (a : xs)
  e             -> error $ "cons: " <> show e

truthy :: Expr -> Context Bool
truthy expr = eval expr <&> \case
  EBool False -> False
  ENil        -> False
  _           -> True

asEnv :: [Expr] -> [Expr] -> Env
asEnv = fromList .: zipWith (\(ESymbol a) b -> (a, b))
