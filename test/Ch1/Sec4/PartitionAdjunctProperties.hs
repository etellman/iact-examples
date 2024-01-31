module Ch1.Sec4.PartitionAdjunctProperties
  ( S (..),
    T (..),
    checkRightAdjunct,
    checkLeftAdjunct,
    tToSPartition,
    sToTPartition,
  )
where

import Ch1.Partition (samePartition)
import Ch1.Set
  ( closureBy,
    overlapsBy,
    sameElementsBy,
  )
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import TestLib.Assertions ((==>))

newtype S = S Int deriving (Show, Eq, Ord)

newtype T = T Int deriving (Show, Eq, Ord)

tToSPartition :: [S] -> (S -> T) -> [[T]] -> [[S]]
tToSPartition ss g tp =
  let ssFor t = filter (\s -> g s == t) ss
   in concat $ (fmap . fmap) ssFor tp

sToTPartition :: (S -> T) -> [[S]] -> [[T]]
sToTPartition g sp = closureBy (==) $ (fmap . fmap) g sp

checkLeftAdjunct :: [[S]] -> (S -> T) -> PropertyT IO ()
checkLeftAdjunct sp g = do
  -- set up
  let ss = concat sp

  -- exercise
  let tp = sToTPartition g sp

  -- verify
  H.assert $ sameElementsBy (==) (concat tp) (fmap g ss)
  overlapsBy (==) tp === []

  s1 <- forAll $ Gen.element ss
  s2 <- forAll $ Gen.element ss
  cover 10 "same S partition" (samePartition sp s1 s2)

  (samePartition sp s1 s2) ==> samePartition tp (g s1) (g s2)

checkRightAdjunct :: [S] -> [[T]] -> (S -> T) -> PropertyT IO ()
checkRightAdjunct ss tp g = do
  -- exercise
  let sp = tToSPartition ss g tp

  -- verify
  H.assert $ sameElementsBy (==) (concat sp) ss
  overlapsBy (==) sp === []

  s1 <- forAll $ Gen.element ss
  s2 <- forAll $ Gen.element ss
  cover 10 "same S partition" (samePartition sp s1 s2)

  (samePartition sp s1 s2) ==> samePartition tp (g s1) (g s2)
