module Preorder.Quantale
  ( distances,
    multStd3,
  )
where

import Data.Matrix
import Data.Vector as V

multPower :: (Num a, Ord a) => Matrix a -> Int -> Matrix a
multPower x 1 = x
multPower x n = multPower (multStd3 x x) (n - 1)

distances :: (Num a, Ord a) => Matrix a -> Matrix a
distances x = multPower x (nrows x)

combine :: (Num a, Ord a) => Vector a -> Vector a -> a
combine xs ys =
  let zipped = V.zipWith (+) xs ys
   in V.foldr1 min zipped

multStd3 :: (Num a, Ord a) => Matrix a -> Matrix a -> Matrix a
multStd3 x y =
  let rows = Prelude.map (flip getRow x) [1 .. nrows x]
      cols = Prelude.map (flip getCol y) [1 .. ncols y]
      zipped = do
        r <- rows
        c <- cols
        return $ combine r c
   in Data.Matrix.fromList (nrows x) (ncols y) zipped
