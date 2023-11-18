module Ch01.PreorderTest (tests) where

import qualified Ch01.Preorder as P
import Data.List (nub)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_product :: Property
prop_product = property $ do
  -- set up
  xs <- forAll $ Gen.list (Range.constant 1 5) (Gen.int $ Range.constant 0 1000)
  ys <- forAll $ Gen.list (Range.constant 1 5) Gen.alpha

  -- exercise
  let (P.Preorder h xys) = P.productPreorder (P.Preorder (<=) xs) (P.Preorder (<=) ys)

  -- verify
  (x1, y1) <- forAll $ Gen.element xys
  (x2, y2) <- forAll $ Gen.element xys

  (x1 <= x2 && y1 <= y2) === h (x1, y1) (x2, y2)

prop_opposite :: Property
prop_opposite = property $ do
  -- set up
  xs <- forAll $ nub <$> Gen.list (Range.constant 1 10) Gen.alpha
  let po = P.Preorder (<=) xs

  -- exercise
  let (P.Preorder oplte opelements) = P.opposite po

  -- verify
  x <- forAll $ Gen.element (xs)
  y <- forAll $ Gen.element (xs)

  (x <= y) === (oplte y x)
  opelements === xs

tests :: TestTree
tests =
  testGroup
    "Ch01.PreorderTest"
    [ testProperty "product" prop_product,
      testProperty "opposite" prop_opposite
    ]
