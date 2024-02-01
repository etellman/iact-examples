module Ch1.Sec4.Exercise100Test (tests) where

import Ch1.Partition (isFiner, partitions)
import Ch1.Sec4.PartitionAdjunctProperties
import Data.Containers.ListUtils (nubOrd)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions ((==>))

genSs :: Gen [S]
genSs =
  fmap S . toList
    <$> Gen.set
      (Range.constant 2 10)
      (Gen.int (Range.linear 0 1000))

g :: Int -> S -> T
g n (S s) = T (s `rem` n)

prop_left :: Property
prop_left = property $ do
  -- set up
  sPartitions <- forAll $ partitions <$> genSs
  n <- forAll $ Gen.int (Range.constant 2 20)

  let toTP = sToTPartition (g n)

  -- exercise and verify
  s1 <- forAll $ Gen.element sPartitions
  s2 <- forAll $ Gen.element sPartitions
  s1 `isFiner` s2 ==> toTP s1 `isFiner` toTP s2

prop_right :: Property
prop_right = property $ do
  -- set up
  ss <- forAll genSs
  n <- forAll $ Gen.int (Range.constant 2 20)

  let tPartitions = partitions . nubOrd . fmap (g n) $ ss
      toSP = tToSPartition ss (g n)

  -- exercise and verify
  t1 <- forAll $ Gen.element tPartitions
  t2 <- forAll $ Gen.element tPartitions
  t1 `isFiner` t2 ==> toSP t1 `isFiner` toSP t2

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Exercise100Test"
    [ testProperty "left" prop_left,
      testProperty "right" prop_right
    ]
