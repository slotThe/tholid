module Builtin (
  builtin,
  progn,
) where

import Interpreter
import Types
import Util

import Control.Monad.Except (throwError)
import Data.IORef (IORef, newIORef)


builtin :: IO (IORef Env)
builtin = newIORef $ fromList
  [ -- essential
    ("progn", EFun progn)
    -- arithmetic
  , ("+"   , EFun $ nAryOp "+" foldl' 0 (+))
  , ("-"   , EFun $ nAryOp "-" foldr  0 (-))
  , ("*"   , EFun $ nAryOp "*" foldl' 1 (*))
  , ("/"   , EFun $ nAryOp "/" foldr  1 div)
    -- comparisons
  , ("^"   , EFun $ nAryOp "^" foldr  1 (^)    )
  , ("<"   , EFun $ fmap EBool         . lt "<")
  , (">"   , EFun $ fmap (EBool . not) . lt ">")
  , ("="   , EFun $ foldM1 eq                  )
    -- list primitives
  , ("list", EFun $ pure . EList)
  , ("car" , EFun car           )
  , ("cdr" , EFun cdr           )
  , ("cons", EFun consList      )
  ]

nAryOp :: forall a b m. ErrorContext m
       => Text -> (a -> b -> [Int] -> Int) -> b -> a -> [Expr] -> m Expr
nAryOp opName cata def op xs = EInt . cata op def <$> traverse unNumber xs
 where
  unNumber :: Expr -> m Int
  unNumber expr = case expr of
    EInt n -> pure n
    _      -> throwError $ BuiltinTypeError opName "number" expr

eq :: MonadContext m => Expr -> Expr -> m Expr
eq (EInt x)    (EInt y)    = pure . EBool $ x == y
eq (ESymbol x) (ESymbol y) = pure . EBool $ x == y
eq (EBool x)   (EBool y)   = pure . EBool $ x == y
eq ENil        ENil        = pure . EBool $ True
eq (EList xs)  (EList ys)  = do
  res <- zipWithM eq xs ys
  EBool . and <$> traverse (fmap (True ==) . truthy) res
eq _ _ = pure $ EBool False

lt :: ErrorContext m => Text -> [Expr] -> m Bool
lt opName = go
 where
  go :: ErrorContext m => [Expr] -> m Bool
  go = \case
    (EInt x : EInt y : xs) -> ((x < y) &&) <$> go xs
    [_]                    -> pure True
    []                     -> pure True
    e                      -> throwError $ BuiltinTypeError opName "(list of) number(s)" e

car :: ErrorContext m => [Expr] -> m Expr
car = \case
  [EList (x : _)] -> pure x
  [EList{}]       -> pure ENil
  e               -> throwError $ BuiltinTypeError "car" "list" e

cdr :: ErrorContext m => [Expr] -> m Expr
cdr = \case
  [EList (_ : xs)] -> pure (EList xs)
  [EList {}]       -> pure ENil
  e                -> throwError $ BuiltinTypeError "cdr" "list" e

consList :: ErrorContext m => [Expr] -> m Expr
consList = \case
  [a, EList xs] -> pure $ EList (a : xs)
  e             -> throwError $ BuiltinTypeError "cons" "element followed by list" e

progn :: MonadContext m => [Expr] -> m Expr
progn = fmap (exprHead . reverse) . traverse eval
