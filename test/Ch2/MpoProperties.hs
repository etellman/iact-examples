module Ch2.MpoProperties
  ( monoid,
    prop_monotonicity,
    prop_symmetry,
  )
where

import Hedgehog as H
import Lib.Preorder
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_monotonicity :: (Show a, Monoid a, Preorder a) => Gen a -> Property
prop_monotonicity gen = property $ do
  -- set up
  x1 <- forAll gen
  x2 <- forAll gen
  y1 <- forAll gen
  y2 <- forAll gen

  -- exercise and verify
  x1 `lte` y1 && x2 `lte` y2 ==> (x1 <> x2) `lte` (y1 <> y2)

prop_unitality :: (Eq a, Show a, Monoid a) => Gen a -> Property
prop_unitality gen = property $ do
  -- set up
  x <- forAll gen

  -- exercise and verify
  x <> mempty === x
  mempty <> x === x

prop_associativity :: (Eq a, Show a, Monoid a) => Gen a -> Property
prop_associativity gen = property $ do
  -- set up
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  -- exercise and verify
  (x <> y) <> z === x <> (y <> z)

prop_symmetry :: (Eq a, Show a, Monoid a) => Gen a -> Property
prop_symmetry gen = property $ do
  -- set up
  x <- forAll gen
  y <- forAll gen

  -- exercise and verify
  x <> y === y <> x

monoid :: (Eq a, Show a, Monoid a) => String -> Gen a -> TestTree
monoid name gen =
  testGroup
    name
    [ testProperty "unitality" $ prop_unitality gen,
      testProperty "associativity" $ prop_associativity gen
    ]
