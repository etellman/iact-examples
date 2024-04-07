module Ch3.Sec5.Example73Test (tests) where

import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions ((==>))

prop_product :: Property
prop_product = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant 1 100)
  y <- forAll $ Gen.int (Range.constant 1 100)
  z' <- forAll $ Gen.int (Range.constant 1 100)

  -- exercise
  let z = min x y

  -- verify
  H.assert $ z <= x -- map from z to x
  H.assert $ z <= y -- map from z to y
  z' <= x && z' <= y ==> z' <= z -- if map from z' to both x and y then map from z' to z

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec5.Example73Test"
    [ testProperty "product" prop_product
    ]
