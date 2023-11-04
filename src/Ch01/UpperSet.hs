module Ch01.UpperSet
  ( upperSets,
  )
where

import Ch01.Preorder
import Data.List (nub)

isUpperSet :: Eq a => (a -> a -> Bool) -> [a] -> [a] -> Bool
isUpperSet gte elements set = all (\x -> x `elem` set || (not $ any (gte x) set)) elements

upperSets :: Eq a => Preorder a -> [[a]] -> [[a]]
upperSets (Preorder gte elements) xss = filter (isUpperSet gte elements) xss
