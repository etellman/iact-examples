module Ch1.MonotoneMap
  ( arrow,
    arrowMonotoneMap,
  )
where

import Preorder.Preorder as PO

-- from exercise 61
arrow :: Preorder a => a -> [a] -> [a]
arrow p = filter (p PO.<=)

arrowMonotoneMap :: Preorder a => [a] -> [[a]]
arrowMonotoneMap xs = fmap (`arrow` xs) xs
