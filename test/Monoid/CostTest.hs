module Monoid.CostTest (tests) where

import Data.Matrix
import Gen.Cost
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Monoid.Cost
import Preorder.Quantale
import Test.Tasty
import Test.Tasty.HUnit
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
  v1 <- forAll $ Gen.realFloat $ Range.exponentialFloat 0 1e6
  v2 <- forAll $ Gen.realFloat $ Range.exponentialFloat (v1 + 1) 1e6

  -- exercise and verify
  H.assert $ Cost v1 <= Cost v2

tests :: TestTree
tests =
  testGroup
    "Monoid.CostTest"
    [ testGroup
        "Ord"
        [ testProperty "all costs are less than or equal to infinity" prop_lessThanInfinity,
          testProperty "finite costs" prop_lessThanInfinity
        ]
    ]
