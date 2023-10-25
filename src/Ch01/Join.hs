module Ch01.Join
  ( simplify,
    partitions,
    join,
    disjoint,
    connected,
    System (..),
    sets,
    elements,
    distribute,
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

instance Functor System where
  fmap :: (a -> b) -> System a -> System b
  fmap f (System xss) = System ((fmap . fmap) f xss)

distribute :: a -> [[a]] -> [[[a]]]
distribute x [] = [[[x]]]
distribute x (xs : xss) =
  -- 'a' ("bc" : ["de", "fg"])
  let firstWithX = ((x : xs) : xss) -- ["abc", "de", "fg"]
      restWithX = distribute x xss -- [["ade"], ["afg"]]
      prependXs = map (xs :) restWithX -- [[bc, "ade"], [bc, "afg"]]
   in firstWithX : prependXs -- [["abc", "de", "fg"], [[bc, "ade"], [bc, "afg"]]]

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x : xs) = do
  yss <- partitions xs
  ys <- distribute x yss
  return ys

-- or
-- partitions (x : xs) = [ys | yss <- partitions xs, ys <- distribute x yss]

-- or
-- partitions = foldr (\x r -> r >>= distribute x) [[]]

-- determines whether all the groups contain different elements
disjoint :: Eq a => System a -> Bool
disjoint (System (xss)) =
  let elems = concat xss
   in (length . nub) elems == length elems

join :: Ord a => System a -> System a -> System a
join (System xss) (System yss) = simplify $ System (xss ++ yss)

simplify :: Ord a => System a -> System a
simplify (System []) = (System [])
simplify s@(System (x : xss))
  | disjoint s = System $ sort $ fmap sort (x : xss)
  | otherwise =
      let (nonOverlapping, overlapping) = partition (\y -> null (intersect x y)) xss
          withX = if null overlapping then [x] else fmap (union x) overlapping
       in simplify (System $ nonOverlapping ++ withX)

-- determines whether two numbers are in the same set
connected :: Eq a => a -> a -> System a -> Bool
connected x y (System xss) =
  let groupFor n = head $ filter (elem n) xss
   in groupFor x == groupFor y
