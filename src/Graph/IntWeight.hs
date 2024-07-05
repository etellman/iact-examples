{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph.IntWeight
  ( IntWeight,
    fromIntWeight,
    toIntWeight,
    unitWeight,
    toCost,
  )
where

import Data.Monoid (Sum (..))
import Monoid.Cost

newtype IntWeight = IntWeight (Sum Int) deriving (Semigroup, Monoid, Eq, Ord, Show)

fromIntWeight :: IntWeight -> Int
fromIntWeight (IntWeight (Sum x)) = x

toIntWeight :: Int -> IntWeight
toIntWeight x = IntWeight $ Sum x

unitWeight :: IntWeight
unitWeight = toIntWeight 1

toCost :: IntWeight -> Cost Int
toCost (IntWeight (Sum x)) = Cost x
