module Ch1.Sec4.Exercise97Test (tests) where

import Ch1.Partition (partitions)
import Ch1.Sec4.PartitionAdjunctProperties
import Data.List (nub)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genSs :: Gen [S]
genSs =
  fmap S
    <$> toList
    <$> Gen.set
      (Range.constant 2 10)
      (Gen.int $ (Range.linear 0 1000))

g :: Int -> S -> T
g n (S s) = T (s `rem` n)

prop_left :: Property
prop_left = property $ do
  -- set up
  ss <- forAll genSs
  sss <- forAll $ Gen.element (partitions ss)
  n <- forAll $ Gen.int (Range.constant 2 20)

  -- exercise
  checkLeftAdjunct sss (g n)

prop_right :: Property
prop_right = property $ do
  -- set up
  ss <- forAll genSs
  n <- forAll $ Gen.int (Range.constant 2 20)
  tss <- forAll $ Gen.element $ (partitions . nub . fmap (g n)) ss

  -- exercise
  checkRightAdjunct ss tss (g n)

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Exercise97Test"
    [ testProperty "left" prop_left,
      testProperty "right" prop_right
    ]
