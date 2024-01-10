{-# LANGUAGE InstanceSigs #-}

module Ch1.SetSystem
  ( SetSystem (..),
    simplify,
    partitions,
    disjoint,
    connected,
    sets,
    elements,
    labelFor,
  )
where

import Ch1.Joinable
import qualified Ch1.Partition as P
import Data.List
  ( intersect,
    nub,
    partition,
    sort,
    union,
  )
import qualified Data.PartialOrd as PO

newtype SetSystem a = SetSystem [[a]] deriving (Eq, Show)

sets :: SetSystem a -> [[a]]
sets (SetSystem xss) = xss

elements :: Ord a => SetSystem a -> [a]
elements (SetSystem xss) = (sort . nub . concat) xss

instance Eq a => PO.PartialOrd (SetSystem a) where
  sx@(SetSystem xss) <= sy =
    let elems = (nub . concat) xss
        pairs = (,) <$> elems <*> elems
        connectionMatches (x, y) = not (connected x y sx) || connected x y sy
     in all connectionMatches pairs

instance Functor SetSystem where
  fmap :: (a -> b) -> SetSystem a -> SetSystem b
  fmap f (SetSystem xss) = SetSystem ((fmap . fmap) f xss)

partitions :: [a] -> [SetSystem a]
partitions xs = fmap SetSystem (P.partitions xs)

labelFor :: Eq a => SetSystem a -> a -> Int
labelFor (SetSystem xss) x = P.labelFor xss x

-- determines whether all the groups contain different elements
disjoint :: Eq a => SetSystem a -> Bool
disjoint (SetSystem (xss)) =
  let elems = concat xss
   in (length . nub) elems == length elems

instance Ord a => Joinable (SetSystem a) where
  join (SetSystem xss) (SetSystem yss) = simplify $ SetSystem (xss ++ yss)

simplify :: Ord a => SetSystem a -> SetSystem a
simplify (SetSystem []) = (SetSystem [])
simplify s@(SetSystem (x : xss))
  | disjoint s = SetSystem $ sort $ fmap sort (x : xss)
  | otherwise =
      let (nonOverlapping, overlapping) = partition (\y -> null (intersect x y)) xss
          withX = if null overlapping then [x] else fmap (union x) overlapping
       in simplify (SetSystem $ nonOverlapping ++ withX)

-- determines whether two numbers are in the same set
connected :: Eq a => a -> a -> SetSystem a -> Bool
connected x y (SetSystem xss) = P.samePartition xss x y
