module Ch1.MonotoneMap
  ( arrow,
    arrowMonotoneMap,
  )
where

import Preorder.Preorder as PO

-- from exercise 61
arrow :: Preorder a => a -> [a] -> [a]
arrow p xs = filter (p PO.<=) xs

arrowMonotoneMap :: Preorder a => [a] -> [[a]]
arrowMonotoneMap xs = fmap (\p -> arrow p xs) xs
