module Ch1.UpperSet
  ( upperSets,
    isUpperSet,
  )
where

import Ch1.Set (powerSet)
import Lib.Preorder

isUpperSet :: (Preorder a, Eq a) => [a] -> [a] -> Bool
isUpperSet elements usCandidate =
  all (\p -> (all (\q -> q `elem` usCandidate || (not $ p `lte` q)) elements)) usCandidate

upperSets :: (Eq a, Preorder a) => [a] -> [[a]]
upperSets elements = filter (isUpperSet elements) (powerSet elements)
