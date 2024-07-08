module Preorder.Quantale
  ( distances,
    quantMult,
    quantPower,
  )
where

import Data.Matrix
import Data.Vector as V

quantPower :: (Semigroup a, Ord a) => Matrix a -> Int -> Matrix a
quantPower x 1 = x
quantPower x n = quantPower (quantMult x x) (n - 1)

distances :: (Semigroup a, Ord a) => Matrix a -> Matrix a
distances x = quantPower x (nrows x)

combine :: (Semigroup a, Ord a) => Vector a -> Vector a -> a
combine xs ys =
  let zipped = V.zipWith (<>) xs ys
   in V.foldr1 min zipped

quantMult :: (Semigroup a, Ord a) => Matrix a -> Matrix a -> Matrix a
quantMult x y =
  let rows = Prelude.map (flip getRow x) [1 .. nrows x]
      cols = Prelude.map (flip getCol y) [1 .. ncols y]
      zipped = do
        r <- rows
        c <- cols
        return $ combine r c
   in Data.Matrix.fromList (nrows x) (ncols y) zipped
