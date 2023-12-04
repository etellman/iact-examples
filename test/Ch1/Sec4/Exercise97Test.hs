module Ch1.Sec4.Exercise97Test (tests) where

import Ch1.Partition (partitionFor, partitions)
import Ch1.Set
  ( closureBy,
    overlapsBy,
    sameElementsBy,
  )
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions ((==>))

newtype S = S Int deriving (Show, Eq, Ord)

newtype T = T Int deriving (Show, Eq, Ord)

-- | verifies that g creates a Galois connection between S and T
checkGaloisConnection :: [[S]] -> (S -> T) -> PropertyT IO ()
checkGaloisConnection sss g = do
  -- exercise
  let tss = closureBy (==) $ (fmap . fmap) g sss
      ss = concat sss

  -- verify
  s1 <- forAll $ Gen.element ss
  s2 <- forAll $ Gen.element ss

  let samePartition xss x1 x2 = partitionFor xss x1 == partitionFor xss x2
  cover 20 "same S partition" (samePartition sss s1 s2)

  -- tss is a partition of T
  H.assert $ sameElementsBy (==) (concat tss) (fmap g ss)
  overlapsBy (==) tss === []

  -- everything in S is in the right place in T
  (samePartition sss s1 s2) ==> samePartition tss (g s1) (g s2)

prop_example96 :: Property
prop_example96 = property $ do
  -- set up
  let sss = [[S 1, S 3], [S 2, S 4]]
      g (S 1) = (T 12)
      g (S 2) = (T 12)
      g (S s) = (T s)

  -- exercise
  checkGaloisConnection sss g

prop_exercise97 :: Property
prop_exercise97 = property $ do
  -- set up
  ss <-
    forAll $
      fmap S
        <$> toList
        <$> Gen.set
          (Range.constant 2 10)
          (Gen.int $ (Range.linear 0 1000))
  sss <- forAll $ Gen.element (partitions ss)

  n <- forAll $ Gen.int (Range.constant 2 20)
  let g (S s) = T (s `rem` n)

  -- exercise
  checkGaloisConnection sss g

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Exercise97Test"
    [ testProperty "example 96" prop_example96,
      testProperty "exercise 97" prop_exercise97
    ]
