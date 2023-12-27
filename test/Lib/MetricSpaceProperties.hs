module Lib.MetricSpaceProperties (metricSpace) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.MetricSpace
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_distanceToSelf :: (Show m, MetricSpace m) => [m] -> Property
prop_distanceToSelf xs = property $ do
  -- set up
  x <- forAll $ Gen.element xs

  -- exercise and verify
  distance x x === 0

prop_equality :: (Show m, Eq m, MetricSpace m) => [m] -> Property
prop_equality xs = property $ do
  -- set up
  x <- forAll $ Gen.element xs
  y <- forAll $ Gen.element xs

  cover 10 "x == y" $ x == y

  -- exercise and verify
  (distance x y == 0) ==> x == y

prop_reflexive :: (Show m, MetricSpace m) => [m] -> Property
prop_reflexive xs = property $ do
  -- set up
  x <- forAll $ Gen.element xs
  y <- forAll $ Gen.element xs

  -- exercise and verify
  distance x y === distance y x

prop_triangle :: (Show m, MetricSpace m) => [m] -> Property
prop_triangle xs = property $ do
  -- set up
  x <- forAll $ Gen.element xs
  y <- forAll $ Gen.element xs
  z <- forAll $ Gen.element xs

  -- exercise and verify
  H.assert $ distance x y + distance y z >= distance x z

metricSpace :: (Eq m, Show m, MetricSpace m) => String -> [m] -> TestTree
metricSpace name xs =
  testGroup
    name
    [ testProperty "distance to self" $ prop_distanceToSelf xs,
      testProperty "0 distance implies equality" $ prop_equality (take 4 xs),
      testProperty "distance doesn't depend on direction" $ prop_reflexive xs,
      testProperty "triangle inequality" $ prop_triangle xs
    ]
