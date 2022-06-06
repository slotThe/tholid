module Run (repl, run, readLisp) where

import Builtin
import Interpreter
import Parser
import Prelude hiding (read)
import Types
import Util

import Data.Text.IO qualified as T

import Control.Exception (SomeException, catch, throwIO)
import Control.Monad.Except (runExceptT)
import System.IO (hFlush, stdout)

repl :: IO ()
repl = do
  env <- builtin
  void . flip runReaderT env . runExceptT . unContext $ do
    traverse_ eval =<< io (readLisp prelude)
    forever do
      io $ putStr "λ> " >> hFlush stdout
      l <- liftIO T.getLine
      liftIO $ withRead () l \exprs ->
        either print print
          =<< (flip runReaderT env . runExceptT . unContext . eval $ head exprs)
                `catch` \(e :: SomeException) -> Right ENil <$ print e

run :: Text -> IO Expr
run = runWith prelude

runWith :: FilePath -> Text -> IO Expr
runWith fp input = do
  fileExprs <- readLisp fp
  withRead ENil input \exprs -> go (fileExprs <> exprs)
 where
  go :: [Expr] -> IO Expr
  go exprs = do
    env <- builtin
    (flip runReaderT env . runExceptT . unContext $ progn exprs) >>= \case
       Left  e    -> throwIO e
       Right expr -> pure expr

readLisp :: FilePath -> IO [Expr]
readLisp fp = do
  fileContents <- T.readFile fp
  withRead [ENil] fileContents pure

withRead :: MonadIO m => a -> Text -> ([Expr] -> m a) -> m a
withRead def str f = case read str of
  Left err     -> def <$ liftIO (putStrLn err)
  Right exprs  -> f exprs

prelude :: String
prelude = "./lisp/prelude.scm"
