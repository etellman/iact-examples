module Ch1.MonotoneMap
  ( arrow,
    arrowMonotoneMap,
  )
where

import Ch1.Preorder (Preorder (..))
import Ch1.Set (isSubsetOf)

-- from exercise 61
arrow :: (a -> a -> Bool) -> a -> [a] -> [a]
arrow lte p = filter (\p' -> p `lte` p')

arrowMonotoneMap :: Eq a => Preorder a -> Preorder [a]
arrowMonotoneMap (Preorder lte xs) =
  let xss = fmap (\p -> arrow lte p xs) xs
   in Preorder isSubsetOf xss