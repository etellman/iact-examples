module Monoid.HomElementTest (tests) where

import Gen.Cost (genFiniteCost)
import Hedgehog as H
import qualified Hedgehog.Range as Range
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_leftInfinity :: Property
prop_leftInfinity = property $ do
  -- set up
  x <- forAll $ genFiniteCost (Range.linear 0 1000)

  -- exercise and verify
  Cost 0 === Infinity -* x

prop_rightInfinity :: Property
prop_rightInfinity = property $ do
  -- set up
  x <- forAll $ genFiniteCost (Range.linear 0 1000)

  -- exercise and verify
  Infinity === x -* Infinity

prop_leftGte :: Property
prop_leftGte = property $ do
  -- set up
  c1@(Cost x) <- forAll $ genFiniteCost (Range.linear 0 1000)
  c2@(Cost y) <- forAll $ genFiniteCost (Range.linear x 2000)

  -- exercise and verify
  Cost (y - x) === c1 -* c2

prop_rightGte :: Property
prop_rightGte = property $ do
  -- set up
  c1@(Cost x) <- forAll $ genFiniteCost (Range.linear 0 1000)
  c2 <- forAll $ genFiniteCost (Range.linear x 2000)

  -- exercise and verify
  Cost 0 === c2 -* c1

tests :: TestTree
tests =
  testGroup
    "Monoid.HomElementTest"
    [ testProperty "left infinity" prop_leftInfinity,
      testProperty "right infinity" prop_rightInfinity,
      testProperty "y >= x" prop_leftGte,
      testProperty "x >= y" prop_rightGte,
      testCase "Infinity -* Infinity" $ Infinity -* Infinity @=? Cost (0 :: Int)
    ]
