module Ch01.Exercise64Test (tests) where

import qualified Ch01.Partition as P
import Data.List (nub)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

data X = X Int deriving (Show, Eq)

data Y = Y Int deriving (Show, Eq)

prop_exercise_64 :: Property
prop_exercise_64 = property $ do
  -- set up
  xs <-
    forAll $
      fmap X
        <$> nub
        <$> Gen.list
          (Range.constant 2 100)
          (Gen.int $ Range.constant 1 100)

  m <- forAll $ Gen.int (Range.constant 2 10)
  let f (X n) = Y (m * n)
      ys = (fmap f xs) :: [Y]

  n1 <- forAll $ Gen.int (Range.constant 2 10)
  let s1 (Y n) = n `rem` n1
      py1 = P.functionToPartition s1 ys
      px1 = P.functionToPartition (s1 . f) xs

  n2 <- forAll $ Gen.int (Range.constant 2 10)
  let s2 (Y n) = n `rem` n2
      py2 = P.functionToPartition s2 ys
      px2 = P.functionToPartition (s2 . f) xs

  py1 `P.isFiner` py2 ==> px1 `P.isFiner` px2

tests :: TestTree
tests =
  testGroup
    "Ch01.Exercise64Test"
    [ testProperty "exercise 64" $ prop_exercise_64
    ]
