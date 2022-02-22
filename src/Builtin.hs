module Builtin (builtin) where

import Interpreter
import Types
import Util

import Data.IORef


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
  go :: [Expr] -> Context Bool
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
