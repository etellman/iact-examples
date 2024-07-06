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
  c <- forAll $ genCost (Range.linear 1 1000)

  -- exercise and verify
  H.assert $ c <= Infinity

prop_lessThanFinite :: Property
prop_lessThanFinite = property $ do
  -- set up
  v1 <- forAll $ Gen.int $ Range.linear 0 100
  v2 <- forAll $ Gen.int $ Range.linear (v1 + 1) 200

  -- exercise and verify
  H.assert $ Cost v1 <= Cost v2

prop_minInfinity :: Property
prop_minInfinity = property $ do
  -- set up
  c <- forAll $ genCost (Range.linear 1 1000)

  -- exercise and verify
  min c Infinity === c

prop_minFinite :: Property
prop_minFinite = property $ do
  -- mi
  v1 <- forAll $ Gen.int $ Range.linear 0 100
  v2 <- forAll $ Gen.int $ Range.linear (v1 + 1) 200

  -- exercise and verify
  min (Cost v1) (Cost v2) === Cost v1

tests :: TestTree
tests =
  testGroup
    "Monoid.CostTest"
    [ testGroup
        "Ord"
        [ testProperty "infinite" prop_lessThanInfinity,
          testProperty "finite" prop_lessThanFinite
        ],
      testGroup
        "min"
        [ testProperty "infinite" prop_minInfinity,
          testProperty "finite" prop_minFinite
        ]
    ]
