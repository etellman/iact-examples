module Ch01.Preorder
  ( Preorder (..),
    productPreorder,
    preorderConnections,
  )
where

import Ch01.Set (cartesianProduct)
import Control.Monad (guard)

data Preorder a = Preorder
  { po_lte :: (a -> a -> Bool),
    po_elements :: [a]
  }

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
