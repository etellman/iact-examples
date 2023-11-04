module Ch01.PreorderTest (tests) where

import Ch01.Preorder
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_product :: Property
prop_product = property $ do
  -- set up
  xs <- forAll $ Gen.list (Range.constant 1 5) (Gen.int $ Range.constant 0 1000)
  ys <- forAll $ Gen.list (Range.constant 1 5) Gen.alpha

  -- exercise
  let (Preorder h xys) = productPreorder (Preorder (<=) xs) (Preorder (<=) ys)

  -- verify
  (x1, y1) <- forAll $ Gen.element xys
  (x2, y2) <- forAll $ Gen.element xys

  x1 <= x2 && y1 <= y2 ==> h (x1, y1) (x2, y2)


tests :: TestTree
tests =
  testGroup
    "Ch01.PreorderTest"
    [ testProperty "product" prop_product
    ]
