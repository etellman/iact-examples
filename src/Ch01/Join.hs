module Ch01.Join
  ( join,
    join2,
    disjoint,
    connected,
    System (..),
    sets,
    elements,
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

sets :: System a -> [[a]]
sets (System xss) = xss

elements :: Ord a => System a -> [a]
elements (System xss) = (sort . nub . concat) xss

instance Eq a => Ord (System a) where
  sx@(System xss) <= sy =
    let elems = (nub . concat) xss
        pairs = (,) <$> elems <*> elems
        connectionMatches (x, y) = not (connected x y sx) || connected x y sy
     in all connectionMatches pairs

-- determines whether all the groups contain different elements
disjoint :: Eq a => System a -> Bool
disjoint (System (xss)) =
  let elems = concat xss
   in (length . nub) elems == length elems

join2 :: Ord a => System a -> System a -> System a
join2 (System xss) (System yss) = join $ System (xss ++ yss)

join :: Ord a => System a -> System a
join (System []) = (System [])
join s@(System (x : xss))
  | disjoint s = System $ sort $ fmap sort (x : xss)
  | otherwise =
      let (nonOverlapping, overlapping) = partition (\y -> null (intersect x y)) xss
          withX = if null overlapping then [x] else fmap (union x) overlapping
       in join (System $ nonOverlapping ++ withX)

-- determines whether two numbers are in the same set
connected :: Eq a => a -> a -> System a -> Bool
connected x y (System xss) =
  let groupFor n = head $ filter (elem n) xss
   in groupFor x == groupFor y
