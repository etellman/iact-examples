module Ch3.Sec5.Example65Test (tests) where

import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_singleton :: Property
prop_singleton = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise
  let f = const ()

  -- verify
  f x === ()

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec5.Example65Test"
    [ testProperty "singleton" prop_singleton
    ]
