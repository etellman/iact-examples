module Ch01.Preorder
  ( Preorder (..),
    productPreorder,
    connections,
    opposite,
    elements,
    isLte,
  )
where

import Ch01.Set (cartesianProduct)
import Control.Monad (guard)

data Preorder a = Preorder (a -> a -> Bool) [a]

isLte :: Eq a => Preorder a -> a -> a -> Bool
isLte (Preorder lte xs) x y = x `lte` y && x `elem` xs && y `elem` xs

elements :: Preorder a -> [a]
elements (Preorder _ xs) = xs

connections :: Eq a => Preorder a -> [(a, a)]
connections (Preorder lte xs) = do
  x <- xs
  y <- xs
  guard $ x `lte` y && x /= y
  return (x, y)

productPreorder :: Preorder a -> Preorder b -> Preorder (a, b)
productPreorder (Preorder f xs) (Preorder g ys) =
  let h (x1, y1) (x2, y2) = f x1 x2 && g y1 y2
   in Preorder h (cartesianProduct xs ys)

opposite :: Preorder a -> Preorder a
opposite (Preorder lte xs) = Preorder (flip lte) xs
