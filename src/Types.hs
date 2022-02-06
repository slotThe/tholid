module Types (
  Env (..),
  Context (..),
  getEnv,
  locally,
  Expr (..),
  io,
  modifyContext,
) where

import qualified Data.Text as T

import Data.Map.Strict (Map)
import Prelude hiding (read)
import Util
import UnliftIO (IORef, MonadUnliftIO, modifyIORef', newIORef, readIORef)

-- | The current environment
newtype Env = Env { unEnv :: Map Text Expr }
  deriving newtype (Semigroup, Monoid, IsList)

-- | Everything happens in a context!
newtype Context a = Context { unContext :: ReaderT (IORef Env) IO a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (IORef Env)
    , MonadIO
    , MonadUnliftIO
    )

-- | TODO
io :: IO a -> Context a
io = liftIO

modifyContext :: (Env -> Env) -> Context ()
modifyContext f = do
  r <- ask
  modifyIORef' r f

-- | TODO
getEnv :: Context Env
getEnv = readIORef =<< ask

-- | Execute a computation with the given local environment.
locally :: Env -> Context Expr -> Context Expr
locally env evalThis = do
  e <- newIORef env
  local (const e) evalThis

-- | TODO
data Expr where
  ENil    :: Expr
  EInt    :: Int -> Expr
  ESymbol :: Text -> Expr
  EList   :: [Expr] -> Expr
  EBool   :: Bool -> Expr
  EFun    ::        ([Expr] -> Context Expr) -> Expr
  ELambda :: Env -> ([Expr] -> Context Expr) -> Expr
             -- ^ 'Env' here are the free variables

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
