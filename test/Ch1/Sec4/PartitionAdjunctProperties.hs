module Ch1.Sec4.PartitionAdjunctProperties
  ( S (..),
    T (..),
    checkRightAdjunct,
    checkLeftAdjunct,
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

checkRightAdjunct :: [S] -> [[T]] -> (S -> T) -> PropertyT IO ()
checkRightAdjunct ss tss g = do
  -- exercise
  let ssFor t = filter (\s -> g s == t) ss
      sss = concat $ (fmap . fmap) ssFor tss

  -- verify
  H.assert $ sameElementsBy (==) (concat sss) ss
  overlapsBy (==) sss === []

  s1 <- forAll $ Gen.element ss
  s2 <- forAll $ Gen.element ss
  cover 20 "same S partition" (samePartition sss s1 s2)

  (samePartition sss s1 s2) ==> samePartition tss (g s1) (g s2)

checkLeftAdjunct :: [[S]] -> (S -> T) -> PropertyT IO ()
checkLeftAdjunct sss g = do
  -- exercise
  let tss = closureBy (==) $ (fmap . fmap) g sss
      ss = concat sss

  -- verify
  H.assert $ sameElementsBy (==) (concat tss) (fmap g ss)
  overlapsBy (==) tss === []

  s1 <- forAll $ Gen.element ss
  s2 <- forAll $ Gen.element ss
  cover 20 "same S partition" (samePartition sss s1 s2)

  (samePartition sss s1 s2) ==> samePartition tss (g s1) (g s2)
