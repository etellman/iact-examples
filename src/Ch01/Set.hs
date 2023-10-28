module Ch01.Set
  ( powerSet,
    cartesianProduct,
  )
where

import Control.Monad (filterM)

powerSet :: [a] -> [[a]]
powerSet = filterM (const [True, False])

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  return (x, y)
