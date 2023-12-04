module Ch1.Set
  ( powerSet,
    isSubsetOf,
    cartesianProduct,
    disjointUnion,
    closureBy,
    overlapsBy,
    sameElementsBy,
  )
where

import Control.Monad (filterM)
import Data.List
  ( intersectBy,
    nubBy,
    partition,
    unionBy,
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

sameElementsBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
sameElementsBy eq xs ys =
  let xs' = nubBy eq xs
      ys' = nubBy eq ys
   in length xs' == length ys' && length xs' == length (intersectBy eq xs' ys')

closureOp :: (a -> a -> Bool) -> [a] -> [[a]] -> [[a]]
closureOp eq xs xss =
  let (without, with) = partition (null . intersectBy eq xs) xss
      merged = foldr (\ys zs -> unionBy eq ys zs) [] with
   in merged : without

closureBy :: (a -> a -> Bool) -> [[a]] -> [[a]]
closureBy _ [] = []
closureBy eq xss =
  let merged = foldr (closureOp eq) xss xss
      eq' = sameElementsBy eq
   in if sameElementsBy eq' merged xss
        then nubBy eq' xss
        else closureBy eq merged

-- | all the sets that share at least one element
overlapsBy :: (a -> a -> Bool) -> [[a]] -> [[a]]
overlapsBy _ [] = []
overlapsBy eq (xs : xss) =
  let filtered = filter (not . null . intersectBy eq xs) xss
      with =
        if null $ filtered
          then []
          else xs : filtered
   in with ++ overlapsBy eq xss
