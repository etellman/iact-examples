module Ch01.Preorder
  ( Preorder (..),
    productPreorder,
    connections,
    opposite,
    isLte,
    isIsomorphic,
    allElements,
  )
where

import Ch01.Set (cartesianProduct)
import Control.Monad (guard)

data Preorder a = Preorder (a -> a -> Bool) [a]

isIsomorphic :: Preorder a -> a -> a -> Bool
isIsomorphic po x y = isLte po x y && isLte po y x

-- | everything in the preorder is less than or equal to x
allElements :: Preorder a -> (a -> Bool) -> Bool
allElements (Preorder _ xs) f = all f xs

isLte :: Preorder a -> a -> a -> Bool
isLte (Preorder lte xs) x y =
  let isElem a = any (\b -> b `lte` a && a `lte` b) xs
   in x `lte` y && isElem x && isElem y

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
