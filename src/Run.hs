module Run (
  repl,
  run,
  readLisp,
) where

import Builtin
import Interpreter
import Parser
import Prelude hiding (read)
import Types
import Util

import Data.Text.IO qualified as T

import Control.Exception (SomeException, catch, throwIO)
import Control.Monad.Except (ExceptT, runExceptT)
import System.IO (hFlush, stdout)


repl :: IO ()
repl = do
  env <- builtin
  void . runContext env $ do
    traverse_ eval =<< io (readLisp prelude)
    forever do
      putStr "λ> " >> hFlush stdout
      l <- T.getLine
      withRead () l \exprs ->
        either print print
          =<< catch (runContext env . eval $ head exprs)
                    \(e :: SomeException) -> Right ENil <$ print e

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
    either throwIO pure <=< runContext env $ progn exprs

readLisp :: FilePath -> IO [Expr]
readLisp fp = do
  fileContents <- T.readFile fp
  withRead [ENil] fileContents pure

withRead :: a -> Text -> ([Expr] -> IO a) -> IO a
withRead def str f = case read str of
  Left err     -> def <$ putStrLn err
  Right exprs  -> f exprs

prelude :: String
prelude = "./lisp/prelude.scm"

runContext :: env -> ExceptT e (ReaderT env m) a -> m (Either e a)
runContext env = flip runReaderT env . runExceptT
