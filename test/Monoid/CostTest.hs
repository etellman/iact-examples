module Monoid.CostTest (tests) where

import Gen.Cost
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Monoid.Cost
import Test.Tasty
import Test.Tasty.Hedgehog

prop_lessThanInfinity :: Property
prop_lessThanInfinity = property $ do
  -- set up
  c <- forAll $ genCost

  -- exercise and verify
  H.assert $ c <= Infinity

prop_lessThanFinite :: Property
prop_lessThanFinite = property $ do
  -- set up
  v1 <- forAll $ Gen.int $ Range.linear 0 100
  v2 <- forAll $ Gen.int $ Range.linear (v1 + 1) 200

  -- exercise and verify
  H.assert $ Cost v1 <= Cost v2

tests :: TestTree
tests =
  testGroup
    "Monoid.CostTest"
    [ testGroup
        "Ord"
        [ testProperty "all costs are less than or equal to infinity" prop_lessThanInfinity,
          testProperty "finite costs" prop_lessThanFinite
        ]
    ]
