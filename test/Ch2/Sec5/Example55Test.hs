module Ch2.Sec5.Example55Test (tests) where

import Gen.Cost (genCost)
import Hedgehog as H
import Monoid.Cost
import Test.Tasty
import Test.Tasty.Hedgehog

prop_monoidalClosed :: Property
prop_monoidalClosed = property $ do
  -- set up
  v <- forAll $ genCost
  w <- forAll $ genCost
  a <- forAll $ genCost

  -- exercise and verify
  (a <> v <= w) === (a <= v -* w)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec5.Example55Test"
    [ testProperty "monoidal closed" prop_monoidalClosed
    ]
