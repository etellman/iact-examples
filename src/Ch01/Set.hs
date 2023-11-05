module Ch01.Set
  ( powerSet,
    cartesianProduct,
    disjointUnion,
  )
where

import Control.Monad (filterM)

powerSet :: [a] -> [[a]]
powerSet = filterM (const [False, True])

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  return (x, y)

disjointUnion :: [a] -> [a] -> [(Int, a)]
disjointUnion xs ys = fmap ((,) 1) xs ++ fmap ((,) 2) ys
