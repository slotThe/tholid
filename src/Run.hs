module Run (repl, run, readLisp) where

import Interpreter
import Parser
import Prelude hiding (read)
import Types
import Util

import qualified Data.Text.IO as T

import UnliftIO.Exception (catch, SomeException)


repl :: IO ()
repl = do
  env <- builtin
  flip runReaderT env . unContext $ forever $ do
    io $ putStr "λ> "
    liftIO T.getLine <&> read >>= \case
       Left err -> io $ print err
       Right s  -> do
         e <- eval (head s) `catch` \(e :: SomeException) -> ENil <$ io (print e)
         io $ print e

run :: [Expr] -> IO Expr
run expr = do
  env <- builtin
  flip runReaderT env . unContext $ go expr
 where
  go [e]      = eval e
  go (e : es) = eval e >> go es
  go []       = pure ENil

readLisp :: FilePath -> IO (Either String [Expr])
readLisp fp = read <$> T.readFile fp
