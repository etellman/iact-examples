module Ch1.Sec2.Exercise64Test (tests) where

import qualified Ch1.Partition as P
import Data.List (nub)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype X = X Int deriving (Show, Eq, Ord)

newtype Y = Y Int deriving (Show, Eq, Ord)

genXs :: Gen [X]
genXs =
  fmap X
    <$> nub
    <$> Gen.list
      (Range.constant 2 100)
      (Gen.int $ Range.constant 1 500)

partitionPair :: [X] -> [Y] -> (X -> Y) -> Int -> ([[X]], [[Y]])
partitionPair xs ys f n =
  let s (Y y) = y `rem` n
      px = P.functionToPartition (s . f) xs
      py = P.functionToPartition s ys
   in (px, py)

prop_exercise_64 :: Property
prop_exercise_64 = property $ do
  -- set up
  xs <- forAll genXs
  m <- forAll $ Gen.int (Range.constant 1 7)
  n1 <- forAll $ Gen.int (Range.constant 1 7)
  n2 <- forAll $ Gen.int (Range.constant 1 7)

  let f (X x) = Y (m * x)
      ys = (fmap f xs)
      pp = partitionPair xs ys f

  -- exercise
  let (px1, py1) = pp n1
      (px2, py2) = pp n2

  -- verify
  cover 20 "finer" $ py1 `P.isFiner` py2
  cover 20 "not finer" $ not (py1 `P.isFiner` py2)

  py1 `P.isFiner` py2 ==> px1 `P.isFiner` px2

tests :: TestTree
tests = testProperty "Ch1.Sec2.Exercise64Test" prop_exercise_64
