module Ch1.Set
  ( powerSet,
    isSubsetOf,
    cartesianProduct,
    disjointUnion,
    closure,
    overlaps,
  )
where

import Control.Monad (filterM)
import Data.List
  ( intersect,
    nub,
    partition,
    sort,
    union,
  )

powerSet :: [a] -> [[a]]
powerSet = filterM (const [False, True])

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  return (x, y)

disjointUnion :: [a] -> [a] -> [(Int, a)]
disjointUnion xs ys = fmap ((,) 1) xs ++ fmap ((,) 2) ys

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf xs ys = all (flip elem ys) xs

op :: Ord a => [a] -> [[a]] -> [[a]]
op xs xss =
  let (with, without) = partition (not . null . intersect xs) xss
      merged = foldr (\ys zs -> sort $ union ys zs) [] with
   in merged : without

closure :: Ord a => [[a]] -> [[a]]
closure [] = []
closure xss =
  let xss' = sort $ fmap sort $ nub xss
      merged = foldr op xss' xss'
   in if (merged `intersect` xss' == xss') then xss' else closure merged

-- | all the sets that share at least one element
overlaps :: Eq a => [[a]] -> [[a]]
overlaps [] = []
overlaps (xs : xss) =
  let filtered = filter (not . null . intersect xs) xss
      with =
        if null $ filtered
          then []
          else xs : filtered
   in with ++ overlaps xss
