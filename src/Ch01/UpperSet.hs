module Ch01.UpperSet
  ( upperSets,
  )
where

import Data.List (nub)

isUpperSet :: Eq a => (a -> a -> Bool) -> [a] -> [a] -> Bool
isUpperSet gte elements set = all (\x -> x `elem` set || (not $ any (gte x) set)) elements

upperSets :: Eq a => (a -> a -> Bool) -> [[a]] -> [[a]]
upperSets gte xss =
  let elements = (nub . concat) xss
   in filter (isUpperSet gte elements) xss
