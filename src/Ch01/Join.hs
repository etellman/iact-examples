module Ch01.Join
  ( join,
    join2,
    disjoint,
  )
where

import Data.List
  ( intersect,
    partition,
    sort,
    union,
  )

disjoint :: Eq a => [[a]] -> Bool
disjoint [] = True
disjoint (x : xs) = all null (fmap (intersect x) xs) && disjoint xs

join2 :: Ord a => [[a]] -> [[a]] -> [[a]]
join2 xs ys = join $ xs ++ ys

join :: Ord a => [[a]] -> [[a]]
join [] = []
join xs'@(x : xs)
  | disjoint xs' = sort $ fmap sort xs'
  | otherwise =
      let (nonOverlapping, overlapping) = partition (\y -> null (intersect x y)) xs
          withX = if null overlapping then [x] else fmap (union x) overlapping
       in join (nonOverlapping ++ withX)
