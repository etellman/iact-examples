module Graph.Arrow
  ( Arrow (..),
    IntWeight,
    fromIntWeight,
    toIntWeight,
    unitWeight,
  )
where

import Data.Maybe (fromJust, isJust)
import Data.Monoid (Sum (..))

newtype IntWeight = IntWeight (Sum Int) deriving (Semigroup, Monoid, Eq, Ord, Show)

fromIntWeight :: IntWeight -> Int
fromIntWeight (IntWeight (Sum x)) = x

toIntWeight :: Int -> IntWeight
toIntWeight x = IntWeight $ Sum x

unitWeight :: IntWeight
unitWeight = toIntWeight 1

class Arrow a v w | a -> v w where
  source :: a -> v
  target :: a -> v
  weight :: a -> w
