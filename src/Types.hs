module Types (
  Env (..),
  Context (..),
  getEnv,
  locally,
  Expr (..),
  io,
  modifyContext,
  insert,
  exprHead,
  -- * Errors
  MonadContext,
  TholidError(..),
) where

import Util
import Prelude hiding (read)

import Data.Text qualified as T

import Control.Exception (Exception)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Map.Strict (Map)


-- | The current environment
newtype Env = Env { unEnv :: Map Text Expr }
  deriving newtype (Semigroup, Monoid, IsList, Show)

-- | Everything happens in a context!
type MonadContext :: (Type -> Type) -> Constraint
type MonadContext m = (MonadError TholidError m, MonadReader (IORef Env) m, MonadIO m)

newtype Context a = Context
  { unContext :: ExceptT TholidError (ReaderT (IORef Env) IO) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (IORef Env)
    , MonadIO
    , MonadError TholidError
    )

-- | Lift an 'IO' action into the 'Context' monad.
io :: IO a -> Context a
io = liftIO

-- | Modify the current environment.
modifyContext :: MonadContext m => (Env -> Env) -> m ()
modifyContext f = do
  r <- ask
  liftIO $ modifyIORef' r f
{-# SPECIALISE modifyContext :: (Env -> Env) -> Context () #-}

-- | Return the current environment.
getEnv :: MonadContext m => m Env
getEnv = liftIO . readIORef =<< ask
{-# SPECIALISE getEnv :: Context Env #-}

-- | Insert a new element into an 'Env'.
insert :: (Text, Expr) -> Env -> Env
insert kv e = fromList [kv] <> e

-- | Execute a computation with the given local environment.
locally :: MonadContext m => Env -> m Expr -> m Expr
locally env evalThis = do
  e <- liftIO $ newIORef env
  local (const e) evalThis
{-# SPECIALISE locally :: Env -> Context Expr -> Context Expr #-}

-- | Get the head of a list of expressions, or nil.
exprHead :: [Expr] -> Expr
exprHead = fromMaybe ENil . listToMaybe

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

-----------------------------------------------------------------------
--- Errors

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
