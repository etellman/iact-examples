module Preorder.Quantale (EdgeCost (..), multPower) where

import Data.Matrix
import Monoid.Cost
import Numeric.Natural

multPower :: Num a => Matrix a -> Natural -> Matrix a
multPower x 1 = x
multPower x n = multPower (multStd x x) (n - 1)

data EdgeCost = EdgeCost (Cost Int) deriving (Eq, Ord)

instance Show EdgeCost where
  show (EdgeCost x) = show x

instance Num EdgeCost where
  (EdgeCost x) * (EdgeCost y) = EdgeCost (x + y)

  (EdgeCost 0) + x = x
  x + (EdgeCost 0) = x
  x + y = min x y

  abs = id
  signum _ = 1
  negate = error "negate not supported"
  fromInteger x = EdgeCost $ fromInteger x
