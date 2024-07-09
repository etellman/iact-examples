module Preorder.Quantale
  ( distances,
    quantMult,
    quantPower,
    distanceFunc,
  )
where

import Data.Matrix
import Data.Vector as V

-- power with repeated multiplication
quantPower :: (Semigroup a, Ord a) => Matrix a -> Int -> Matrix a
quantPower x n = Prelude.foldr quantMult x (Prelude.take (n - 1) (repeat x))

-- the shortest distances between any two vertices
distances :: (Semigroup a, Ord a) => Matrix a -> Matrix a
distances x = quantPower x (nrows x)

-- creates a function that returns the distance between two vertices
distanceFunc :: (Semigroup a, Ord a) => Matrix a -> (v -> Int) -> (v -> v -> a)
distanceFunc ws indexOf =
  let f x y = getElem (indexOf x) (indexOf y) (distances ws)
   in f

combine :: (Semigroup a, Ord a) => Vector a -> Vector a -> a
combine xs ys =
  let zipped = V.zipWith (<>) xs ys
   in V.foldr1 min zipped

-- matrix multiplication of Quantales
quantMult :: (Semigroup a, Ord a) => Matrix a -> Matrix a -> Matrix a
quantMult x y =
  let rows = Prelude.map (flip getRow x) [1 .. nrows x]
      cols = Prelude.map (flip getCol y) [1 .. ncols y]
      zipped = do
        r <- rows
        c <- cols
        return $ combine r c
   in Data.Matrix.fromList (nrows x) (ncols y) zipped
