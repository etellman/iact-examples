module Ch01.MonotoneMapTest (tests) where

import Ch01.MonotoneMap
import Ch01.Set
import Ch01.UpperSet (isUpperSet)
import Data.List (nub)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_example55 :: Property
prop_example55 = property $ do
  -- set up
  x <- forAll $ Gen.bool
  y <- forAll $ Gen.bool
  let f False = 17 :: Int
      f True = 24

  -- exercise and verify
  x <= y ==> f x <= f y

prop_example57 :: Property
prop_example57 = property $ do
  -- set up
  xss <- forAll $ powerSet <$> nub <$> Gen.list (Range.constant 1 20) Gen.alpha

  xs <- forAll $ Gen.element xss
  ys <- forAll $ Gen.element xss

  -- exercise and verify
  (xs `isSubsetOf` ys) ==> (length xs <= length ys)

prop_exercise61_1 :: Property
prop_exercise61_1 = property $ do
  -- set up
  xss <- forAll $ nub <$> Gen.list (Range.constant 1 100) Gen.alpha
  p <- forAll $ Gen.element xss

  -- exercise
  let ap = arrow (<=) p xss

  -- exercise and verify
  assert $ isUpperSet (>=) ap xss

tests :: TestTree
tests =
  testGroup
    "Ch01.MonotoneMapTest"
    [ testProperty "example 1.55" prop_example55,
      testProperty "example 1.57" prop_example57,
      testGroup
        "exercise 61"
        [ testProperty "part 1" prop_exercise61_1
        ]
    ]
