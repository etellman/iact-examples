module Ch01.Partition
  ( isFiner,
    partitions,
    isPartition,
    distribute,
    partitionFor,
    labelFor,
    functionToPartition,
  )
where

import Ch01.Set (isSubsetOf)
import Data.List
  ( findIndex,
    partition,
    sort,
  )
import Data.Maybe (fromJust)

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

-- | finds the index of a partition
labelFor :: Eq a => [[a]] -> a -> Int
labelFor xss x = fromJust . findIndex (elem x) $ xss

-- | converts a list of lists to a partition function
partitionFor :: Eq a => [[a]] -> a -> [a]
partitionFor xss x = xss !! labelFor xss x

-- | converts a partition function to a list of lists
functionToPartition :: Eq b => (a -> b) -> [a] -> [[a]]
functionToPartition _ [] = []
functionToPartition f (x : xs) =
  let (p1, rest) = partition (\y -> f x == f y) xs
   in (x : p1) : functionToPartition f rest

isPartition :: Ord a => [a] -> [[a]] -> Bool
isPartition xs xss = (sort . concat) xss == sort xs
