module Ch1.UpperSet
  ( upperSets,
    isUpperSet,
  )
where

import Ch1.Set (powerSet)
import Preorder.Preorder as PO

isUpperSet :: (Preorder a, Eq a) => [a] -> [a] -> Bool
isUpperSet elements usCandidate =
  all
    ( \p ->
        all
          (\q -> q `elem` usCandidate || not (p PO.<= q))
          elements
    )
    usCandidate

upperSets :: (Eq a, Preorder a) => [a] -> [[a]]
upperSets elements = filter (isUpperSet elements) (powerSet elements)
