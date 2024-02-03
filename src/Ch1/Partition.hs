module Ch1.Partition
  ( isFiner,
    partitions,
    isPartition,
    distribute,
    partitionFor,
    samePartition,
    labelFor,
    functionToPartition,
  )
where

import Ch1.Set (isSubsetOf)
import Safe (at)
import Data.List
  ( findIndex,
    partition,
    sort,
  )

isFiner :: Eq a => [[a]] -> [[a]] -> Bool
isFiner xss yss = all (\xs -> any (xs `isSubsetOf`) yss) xss

partitions :: [a] -> [[[a]]]
partitions = foldr (\x r -> r >>= distribute x) [[]]

distribute :: a -> [[a]] -> [[[a]]]
distribute x [] = [[[x]]]
distribute x (xs : xss) =
  -- 'a' ("bc" : ["de", "fg"])
  let firstWithX = ((x : xs) : xss) -- ["abc", "de", "fg"]
      restWithX = distribute x xss -- [["ade"], ["afg"]]
      prependXs = map (xs :) restWithX -- [[bc, "ade"], [bc, "afg"]]
   in firstWithX : prependXs -- [["abc", "de", "fg"], [[bc, "ade"], [bc, "afg"]]]

-- | finds the label of a partition
labelFor :: Eq a => [[a]] -> a -> Maybe Int
labelFor xss x = findIndex (elem x) xss

-- | converts a list of lists to a partition function
partitionFor :: Eq a => [[a]] -> a -> Maybe [a]
partitionFor xss x = fmap (xss `at`) (labelFor xss x)

samePartition :: Eq a => [[a]] -> a -> a -> Bool
samePartition xss x1 x2 = partitionFor xss x1 == partitionFor xss x2

-- | converts a partition function to a list of lists
functionToPartition :: Eq b => (a -> b) -> [a] -> [[a]]
functionToPartition _ [] = []
functionToPartition f (x : xs) =
  let (p1, rest) = partition (\y -> f x == f y) xs
   in (x : p1) : functionToPartition f rest

isPartition :: Ord a => [a] -> [[a]] -> Bool
isPartition xs xss = (sort . concat) xss == sort xs
