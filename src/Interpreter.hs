module Interpreter (
  eval,
  truthy,
) where

import Types
import Util

import Control.Monad.Except (ExceptT, throwError)
import Data.IORef (IORef)
import Data.Map.Strict qualified as Map


eval :: forall context. MonadContext context => Expr -> context Expr
eval = \case
  -- Trivial stuff
  ENil      -> pure ENil
  EInt n    -> pure $ EInt n
  EBool b   -> pure $ EBool b
  EList []  -> pure $ EList []
  ESymbol s ->
    getEnv <&> (unEnv >>> Map.lookup s) >>= \case
      Nothing -> throwError $ SymbolNotInScope s
      Just s' -> pure s'

  -- Control structures
  EList (ESymbol "quote" : body) -> pure $ EList body

  EList (ESymbol "syntax-quote" : body) -> EList . concat <$> traverse unquote body

  EList (ESymbol "if" : cond : then' : else') ->
    ifM (truthy cond) (eval then') (eval $ exprHead else')

  EList (ESymbol "cond" : clauses) -> genCond clauses
   where
    evalCondition :: Expr -> context (Maybe Expr)
    evalCondition = \case
      EList [i, t] -> ifM (truthy i) (Just <$> eval t) (pure Nothing)
      _            -> throwError $ CustomError "cond's arguments should be lists of two elements."
    genCond :: [Expr] -> context Expr
    genCond = \case
      []       -> pure ENil
      (x : xs) -> evalCondition x >>= maybe (genCond xs) pure

  EList [ESymbol "let", EList bindings, body] -> do
    env   <- getEnv
    binds <- evalBindings env bindings
    locally (binds <> env) (eval body)
   where
    evalBinding :: Env -> Expr -> context (Text, Expr)
    evalBinding e = \case
      EList [ESymbol name, bindTo] -> (name, ) <$> locally e (eval bindTo)
      _                            -> error "evalBinding"
    evalBindings :: Env -> [Expr] -> context Env
    evalBindings e = \case
      []       -> pure e
      (x : xs) -> evalBinding e x >>= \b -> evalBindings (insert b e) xs

  -- Macros, functions, and applications
  EList [ESymbol "define-syntax", ESymbol name, EList params, body] -> do
    funOrMacro name $ \addFn env ->
      EMacro \args -> do
        let doLocal = locally (insert addFn (asEnv params args <> env))
        doLocal . eval <=< doLocal . eval $ body  -- it's a double eval!

  EList [ESymbol "define", ESymbol name, EList params, body] -> do
    funOrMacro name $ \addFn env ->
      EFun \args ->
        locally (insert addFn (asEnv params args <> env)) (eval body)

  EList [ESymbol "lambda", EList params, body] -> do
    freeVars <- getEnv
    pure $ ELambda freeVars \args ->
      locally (asEnv params args <> freeVars) (eval body)

  EList (f : xs) -> eval f >>= \case
     EMacro      g ->               g =<< traverse eval xs
     EFun        g ->               g =<< traverse eval xs
     ELambda env g -> locally env . g =<< traverse eval xs
     _             -> throwError $ CantApply f xs

  _ -> error "eval"
{-# SPECIALISE eval :: Expr -> ExceptT TholidError (ReaderT (IORef Env) IO) Expr #-}

funOrMacro :: MonadContext m => Text -> ((Text, Expr) -> Env -> Expr) -> m Expr
funOrMacro name execute = do
  env <- getEnv
  let addFn = (name, fun)       -- tying a tiny knot <3
      fun   = execute addFn env
  modifyContext (insert addFn)
  pure fun                      -- oh yeah

unquote :: MonadContext m => Expr -> m [Expr]
unquote = \case
  EList [ESymbol "," , body] -> traverse eval [body]
  EList [ESymbol ",@", body] -> eval body >>= \case
    EList xs -> pure xs
    _        -> pure [body]
  EList body                 -> (:[]) . EList . concat <$> traverse unquote body
  donteval                   -> pure [donteval]

asEnv :: [Expr] -> [Expr] -> Env
asEnv = fromList .: zipWith (\(ESymbol a) b -> (a, b))

truthy :: MonadContext m => Expr -> m Bool
truthy expr = eval expr <&> \case
  EBool False -> False
  ENil        -> False
  _           -> True
