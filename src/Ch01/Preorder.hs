module Ch01.Preorder
  ( Preorder (..),
    productPreorder,
    preorderConnections,
    oppositePreorder,
    preorderElements,
    isLte,
  )
where

import Ch01.Set (cartesianProduct)
import Control.Monad (guard)

data Preorder a = Preorder (a -> a -> Bool) [a]

isLte :: Eq a => Preorder a -> a -> a -> Bool
isLte (Preorder lte xs) x y = x `lte` y && x `elem` xs && y `elem` xs

preorderElements :: Preorder a -> [a]
preorderElements (Preorder _ xs) = xs

preorderConnections :: Eq a => Preorder a -> [(a, a)]
preorderConnections (Preorder lte xs) = do
  x <- xs
  y <- xs
  guard $ x `lte` y && x /= y
  return (x, y)

productPreorder :: Preorder a -> Preorder b -> Preorder (a, b)
productPreorder (Preorder f xs) (Preorder g ys) =
  let h (x1, y1) (x2, y2) = f x1 x2 && g y1 y2
   in Preorder h (cartesianProduct xs ys)

oppositePreorder :: Preorder a -> Preorder a
oppositePreorder (Preorder lte xs) = Preorder (flip lte) xs
