module Run (repl, run, readLisp) where

import Builtin
import Interpreter
import Parser
import Prelude hiding (read)
import Types
import Util

import Data.Text.IO qualified as T

import System.IO (hFlush, stdout)
import UnliftIO.Exception (SomeException, catch)


repl :: IO ()
repl = do
  env <- builtin
  flip runReaderT env . unContext $ do
    traverse_ eval =<< io (readLisp prelude)
    forever do
      io $ putStr "λ> " >> hFlush stdout
      l <- liftIO T.getLine
      withRead () l \exprs -> do
        e <- eval (head exprs)
               `catch` \(e :: SomeException) -> ENil <$ io (print e)
        io $ print e

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
    flip runReaderT env . unContext $ evalExprs exprs

  evalExprs :: [Expr] -> Context Expr
  evalExprs = fmap (fromMaybe ENil . listToMaybe) . traverse eval

readLisp :: FilePath -> IO [Expr]
readLisp fp = do
  fileContents <- T.readFile fp
  withRead [ENil] fileContents pure

withRead :: MonadIO m => a -> Text -> ([Expr] -> m a) -> m a
withRead def str f = case read str of
  Left err     -> def <$ liftIO (print err)
  Right exprs  -> f exprs

prelude :: String
prelude = "./lisp/prelude.scm"
