module Types (
  -- * The underlying environment and context
  Env (..),
  getEnv,
  MonadContext,
  locally,
  modifyContext,
  io,

  -- * Expressions
  Expr (..),
  insert,
  exprHead,

  -- * Errors
  TholidError (..),
) where

import Util
import Prelude hiding (read)

import Data.Text qualified as T

import Control.Exception (Exception)
import Control.Monad.Except (MonadError)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Map.Strict (Map)

-----------------------------------------------------------------------
-- Env and context

-- | The current environment
newtype Env = Env { unEnv :: Map Text Expr }
  deriving newtype (Semigroup, Monoid, IsList, Show)

-- | Return the current environment.
getEnv :: MonadContext m => m Env
getEnv = io . readIORef =<< ask
{-# INLINE getEnv #-}

-- | Everything happens in a context!
type MonadContext :: (Type -> Type) -> Constraint
type MonadContext m = (MonadError TholidError m, MonadReader (IORef Env) m, MonadIO m)

-- | Lift an 'IO' action into some 'MonadContext' context.
io :: MonadContext m => IO a -> m a
io = liftIO

-- | Modify the current environment.
modifyContext :: MonadContext m => (Env -> Env) -> m ()
modifyContext f = do
  r <- ask
  io $ modifyIORef' r f
{-# INLINE modifyContext #-}

-- | Execute a computation with the given local environment.
locally :: MonadContext m => Env -> m Expr -> m Expr
locally env evalThis = do
  e <- io $ newIORef env
  local (const e) evalThis
{-# INLINE locally #-}

-----------------------------------------------------------------------
-- Expressions

-- | A lisp expression.
data Expr where
  ENil    :: Expr
  EInt    :: Int -> Expr
  ESymbol :: Text -> Expr
  EList   :: [Expr] -> Expr
  EBool   :: Bool -> Expr
  EFun    ::        (forall m. MonadContext m => [Expr] -> m Expr) -> Expr
  EMacro  ::        (forall m. MonadContext m => [Expr] -> m Expr) -> Expr
  ELambda :: Env -> (forall m. MonadContext m => [Expr] -> m Expr) -> Expr
             -- ^ 'Env' here are the free variables at the time of evaluation.

instance Show Expr where
  show :: Expr -> String
  show = \case
    ENil      -> "nil"
    EInt n    -> show n
    ESymbol t -> T.unpack t
    EList xs  -> "(" <> unwords (map show xs) <> ")"
    EBool b   -> if b then "#t" else "#f"
    ELambda{} -> "≪lambda≫"
    EFun{}    -> "≪function≫"
    EMacro{}  -> "≪macro≫"

-- | Insert a new expression into an environment.
insert :: (Text, Expr) -> Env -> Env
insert kv e = fromList [kv] <> e

-- | Get the head of a list of expressions, or nil.
exprHead :: [Expr] -> Expr
exprHead = fromMaybe ENil . listToMaybe

-----------------------------------------------------------------------
-- Errors

data TholidError where
  BuiltinTypeError :: Show e => Text -> Text -> e -> TholidError
  SymbolNotInScope :: Text -> TholidError
  CantApply        :: Show e => e -> [e] -> TholidError
  CustomError      :: String -> TholidError
instance Exception TholidError

instance Show TholidError where
  show :: TholidError -> String
  show = \case
    BuiltinTypeError op expect expr ->
      "Type error in builtin function " <> T.unpack op
        <> ", in expression " <> show expr
        <> ": expects " <> T.unpack expect
    SymbolNotInScope s -> "Symbol not in scope: " <> T.unpack s
    CantApply e xs -> "Can't apply " <> show e
                   <> " to arguments " <> unwords (map show xs)
                   <> ": not a function, macro, or lambda."
    CustomError s -> s
