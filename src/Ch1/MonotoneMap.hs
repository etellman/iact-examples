module Ch1.MonotoneMap
  ( arrow,
    arrowMonotoneMap,
  )
where

import Lib.Preorder

-- from exercise 61
arrow :: Preorder a => a -> [a] -> [a]
arrow p xs = filter (lte p) xs

arrowMonotoneMap :: Preorder a => [a] -> [[a]]
arrowMonotoneMap xs = fmap (\p -> arrow p xs) xs
