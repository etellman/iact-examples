module Ch1.MonotoneMap
  ( arrow,
    arrowMonotoneMap,
  )
where

import Data.PartialOrd as PO

-- from exercise 61
arrow :: PartialOrd a => a -> [a] -> [a]
arrow p = filter (p PO.<=)

arrowMonotoneMap :: PartialOrd a => [a] -> [[a]]
arrowMonotoneMap xs = fmap (`arrow` xs) xs
