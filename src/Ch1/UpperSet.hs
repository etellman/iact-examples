module Ch1.UpperSet
  ( upperSets,
    upperSetPreorder,
    isUpperSet,
  )
where

import Ch1.Preorder (Preorder (..))
import Ch1.Set (powerSet)

isUpperSet :: Eq a => (a -> a -> Bool) -> [a] -> [a] -> Bool
isUpperSet lte elements usCandidate =
  all (\p -> (all (\q -> q `elem` usCandidate || (not $ p `lte` q)) elements)) usCandidate

upperSets :: Eq a => Preorder a -> [[a]]
upperSets (Preorder lte elements) = filter (isUpperSet lte elements) (powerSet elements)

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs ys = all (\x -> x `elem` ys) xs

upperSetPreorder :: Eq a => Preorder a -> Preorder [a]
upperSetPreorder po = Preorder isSubset (upperSets po)