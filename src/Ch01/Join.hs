module Ch01.Join
  ( join,
    join2,
    disjoint,
    connected,
    System (..),
  )
where

import Data.List
  ( intersect,
    nub,
    partition,
    sort,
    union,
  )

newtype System a = System [[a]] deriving (Eq, Show)

instance Eq a => Ord (System a) where
  (System xss) <= (System yss) =
    let elems = (nub . concat) xss
        pairs = (,) <$> elems <*> elems
        connectionMatches (x, y) = not (connected x y xss) || connected x y yss
     in all connectionMatches pairs

-- determines whether all the groups contain different elements
disjoint :: Eq a => [[a]] -> Bool
disjoint xs =
  let elems = concat xs
   in (length . nub) elems == length elems

join2 :: Ord a => [[a]] -> [[a]] -> [[a]]
join2 xs ys = join $ xs ++ ys

join :: Ord a => [[a]] -> [[a]]
join [] = []
join xs'@(x : xs)
  | disjoint xs' = sort $ fmap sort xs'
  | otherwise =
      let (nonOverlapping, overlapping) = partition (\y -> null (intersect x y)) xs
          withX = if null overlapping then [x] else fmap (union x) overlapping
       in join (nonOverlapping ++ withX)

-- determines whether two numbers are in the same group
connected :: Eq a => a -> a -> [[a]] -> Bool
connected x y xs =
  let groupFor n = head $ filter (elem n) xs
   in groupFor x == groupFor y
