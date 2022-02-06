module Util (
  module Exports,
  Text,
  ifM,
  (.:),
  foldM1,
  if',
) where

import Control.Applicative  as Exports
import Control.Monad.Reader as Exports
import Data.Either          as Exports
import Data.Foldable        as Exports hiding (toList)
import Data.Functor         as Exports
import Data.Maybe           as Exports
import Data.Void            as Exports
import GHC.Exts             as Exports

import Data.Text (Text)

-- | Like @if-then-else@, but lifted to an arbitrary 'Monad'.
ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM ma mb mc = ma >>= \a -> if a then mb else mc
{-# INLINE ifM #-}

-- | Like @if-then-else@, but as a function.
if' :: Bool -> p -> p -> p
if' i t e = if i then t else e

-- | f .: g ≡ (\f g -> (\a b -> f (g a b)))
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

-- | Like 'foldM', but take the first element of the list as an initial
-- value.
foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f = \case
  []       -> error "foldM1: empty list"
  (x : xs) -> foldM f x xs
