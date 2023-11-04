module Ch01.Preorder
  ( Preorder (..),
    productPreorder,
  )
where

import Ch01.Set (cartesianProduct)

data Preorder a = Preorder (a -> a -> Bool) [a]

productPreorder :: Preorder a -> Preorder b -> Preorder (a, b)
productPreorder (Preorder f xs) (Preorder g ys) =
  let h (x1, y1) (x2, y2) = f x1 x2 && g y1 y2
   in Preorder h (cartesianProduct xs ys)
