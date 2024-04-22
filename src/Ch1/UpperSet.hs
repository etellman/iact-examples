module Ch1.UpperSet
  ( upperSets,
    isUpperSet,
  )
where

import Ch1.Set (powerSet)
import Data.PartialOrd as PO

isUpperSet :: (PartialOrd a, Eq a) => [a] -> [a] -> Bool
isUpperSet elements usCandidate =
  all
    ( \p ->
        all
          (\q -> q `Prelude.elem` usCandidate || not (p PO.<= q))
          elements
    )
    usCandidate

upperSets :: (Eq a, PartialOrd a) => [a] -> [[a]]
upperSets elements = filter (isUpperSet elements) (powerSet elements)
