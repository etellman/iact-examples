module Ch1.PreservingTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_orderPreserving :: Property
prop_orderPreserving =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 0 100)
    y <- forAll $ Gen.int (Range.constant 0 100)
    let f n = n * n

    -- exercise and verify
    x >= y ==> f x >= f y

prop_metricPreserving :: Property
prop_metricPreserving =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    y <- forAll $ Gen.int (Range.constant (-100) 100)
    m <- forAll $ Gen.int (Range.constant (-100) 100)

    let f n = n + m

    -- exercise and verify
    abs (x - y) === abs (f x - f y)

prop_additionPreserving :: Property
prop_additionPreserving =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    y <- forAll $ Gen.int (Range.constant (-100) 100)
    m <- forAll $ Gen.int (Range.constant (-100) 100)

    let f n = n * m

    -- exercise and verify
    f (x + y) === f x + f y

tests :: TestTree
tests =
  testGroup
    "Ch1.PreservingTest"
    [ testProperty "order-preserving" prop_orderPreserving,
      testProperty "metric-preserving" prop_metricPreserving,
      testProperty "addition-preserving" prop_additionPreserving
    ]
