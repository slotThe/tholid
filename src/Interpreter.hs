module Interpreter (
  eval,
  truthy,
) where

import Types
import Util

import Data.Map.Strict qualified as Map
import Data.Text       qualified as T


eval :: Expr -> Context Expr
eval = \case
  -- Trivial stuff
  ENil      -> pure ENil
  EInt n    -> pure $ EInt n
  EBool b   -> pure $ EBool b
  EList []  -> pure $ EList []
  ESymbol s ->
    getEnv <&> (unEnv >>> Map.lookup s) >>= \case
      Nothing -> error $ "symbol not found: " <> T.unpack s
      Just s' -> pure s'

  -- Control structures
  EList (ESymbol "quote" : body) -> pure $ EList body

  EList (ESymbol "syntax-quote" : body) -> EList <$> traverse unquote body

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
     ELambda env g -> do
       args <- traverse eval xs
       locally env (g args)
     e             -> error $ "not a function or a lambda: " <> show e

  _ -> error "eval"

unquote :: Expr -> Context Expr
unquote = \case
  EList [ESymbol ",", body] -> eval body
  donteval                  -> pure donteval

asEnv :: [Expr] -> [Expr] -> Env
asEnv = fromList .: zipWith (\(ESymbol a) b -> (a, b))

truthy :: Expr -> Context Bool
truthy expr = eval expr <&> \case
  EBool False -> False
  ENil        -> False
  _           -> True
