module Types (
  Env (..),
  Context (..),
  getEnv,
  locally,
  Expr (..),
  io,
  modifyContext,
  insert,
) where

import qualified Data.Text as T

import Data.Map.Strict (Map)
import Prelude hiding (read)
import Util
import UnliftIO (IORef, MonadUnliftIO, modifyIORef', newIORef, readIORef)

-- | The current environment
newtype Env = Env { unEnv :: Map Text Expr }
  deriving newtype (Semigroup, Monoid, IsList, Show)

-- | Everything happens in a context!
newtype Context a = Context { unContext :: ReaderT (IORef Env) IO a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (IORef Env)
    , MonadIO
    , MonadUnliftIO
    )

-- | Lift an 'IO' action into the 'Context' monad.
io :: IO a -> Context a
io = liftIO

-- | Modify the current environment.
modifyContext :: (Env -> Env) -> Context ()
modifyContext f = do
  r <- ask
  modifyIORef' r f

-- | Return the current environment.
getEnv :: Context Env
getEnv = readIORef =<< ask

-- | Insert a new element into an 'Env'.
insert :: (Text, Expr) -> Env -> Env
insert kv e = fromList [kv] <> e

-- | Execute a computation with the given local environment.
locally :: Env -> Context Expr -> Context Expr
locally env evalThis = do
  e <- newIORef env
  local (const e) evalThis

-- | A lisp expression.
data Expr where
  ENil    :: Expr
  EInt    :: Int -> Expr
  ESymbol :: Text -> Expr
  EList   :: [Expr] -> Expr
  EBool   :: Bool -> Expr
  EFun    ::        ([Expr] -> Context Expr) -> Expr
  ELambda :: Env -> ([Expr] -> Context Expr) -> Expr
             -- ^ 'Env' here are the free variables at the time of evaluation.

instance Show Expr where
  show :: Expr -> String
  show = \case
    ENil      -> "nil"
    EInt n    -> show n
    ESymbol t -> T.unpack t
    EList xs  -> "(" <> unwords (map show xs) <> ")"
    EBool b   -> if b then "#t" else "#f"
    ELambda{} -> "<lambda>"
    EFun{}    -> "<function>"
