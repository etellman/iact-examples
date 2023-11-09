module Ch01.MonotoneMapTest (tests) where

import Ch01.MonotoneMap
import qualified Ch01.Preorder as P
import Ch01.Set
import Ch01.UpperSet
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

  -- verify
  assert $ isUpperSet (>=) ap xss

prop_exercise61_2 :: Property
prop_exercise61_2 = property $ do
  -- set up
  xs <- forAll $ nub <$> Gen.list (Range.linear 1 20) Gen.alpha
  let po = P.Preorder (<=) xs

  -- exercise
  let arrowPreorder = arrowMonotoneMap po

  -- verify
  assert $ (P.elements arrowPreorder) `isSubsetOf` upperSets po

  p <- forAll $ Gen.element xs
  q <- forAll $ Gen.element xs
  let arrow' x = arrow (<=) x xs

  P.isLte (P.opposite po) p q ==> P.isLte arrowPreorder (arrow' p) (arrow' q)

tests :: TestTree
tests =
  testGroup
    "Ch01.MonotoneMapTest"
    [ testProperty "example 1.55" prop_example55,
      testProperty "example 1.57" prop_example57,
      testGroup
        "exercise 61"
        [ testProperty "part 1" prop_exercise61_1,
          testProperty "part 2" prop_exercise61_2
        ]
    ]
