module Preorder.Preorder (connections) where

import Control.Monad (guard)
import Data.PartialOrd as PO

-- | connections between pairs of elements
connections :: PartialOrd a => [a] -> [(a, a)]
connections xs = do
  x <- xs
  y <- xs
  guard $ x PO.<= y
  return (x, y)
