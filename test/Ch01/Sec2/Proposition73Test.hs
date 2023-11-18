module Ch01.Sec2.Proposition73Test (tests) where

import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_forward :: Property
prop_forward = property $ do
  xs <- forAll $ toList <$> Gen.set (Range.constant 1 20) (Gen.int $ Range.constant 0 100)

  n <- forAll $ Gen.element xs
  let u = filter (>= n) xs

  p <- forAll $ Gen.element u
  q <- forAll $ Gen.element (filter (>= p) xs)

  H.assert $ q `elem` u

prop_reverse :: Property
prop_reverse = property $ do
  xs <- forAll $ toList <$> Gen.set (Range.constant 1 20) (Gen.int $ Range.constant 0 100)

  n <- forAll $ Gen.element xs
  let u = filter (>= n) xs
      fu = flip elem u

  p <- forAll $ Gen.element xs
  q <- forAll $ Gen.element xs

  p <= q ==> (not . fu $ p) || (fu p && fu q)

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec2.Proposition73Test"
    [ testProperty "forward" prop_forward,
      testProperty "reverse" prop_reverse
    ]
