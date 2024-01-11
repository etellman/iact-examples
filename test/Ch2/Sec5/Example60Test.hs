module Ch2.Sec5.Example60Test (tests) where

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

  cover 10 "a <> v >= w" $ a <> v >= w
  cover 10 "a <> v < w" $ a <> v < w
  cover 5 "v == Infinity" $ v == Infinity
  cover 5 "w == Infinity" $ w == Infinity
  cover 5 "a == Infinity" $ a == Infinity

  -- exercise and verify
  (a <> v >= w) === (a >= v -* w)

tests :: TestTree
tests = testGroup "Ch2.Sec5.Example60Test" [testProperty "monoidal closed" prop_monoidalClosed]
