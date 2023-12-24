module Ch2.Sec2.MonoidalMapProperties
  ( laxMonotoneMap,
    strongMonotoneMap,
    strictMonotoneMap,
  )
where

import Hedgehog as H
import Lib.Preorder
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_monoid ::
  (Show p, Monoid p, Monoid q) =>
  Gen p ->
  (p -> q) ->
  (q -> q -> Bool) ->
  Property
prop_monoid gen f cmp = property $ do
  -- set up
  p1 <- forAll gen
  p2 <- forAll gen

  -- exercise and verify
  H.assert $ (f p1 <> f p2) `cmp` f (p1 <> p2)

monoidalMap ::
  (Show p, Monoid p, Monoid q) =>
  String ->
  Gen p ->
  (p -> q) ->
  (q -> q -> Bool) ->
  TestTree
monoidalMap name gen f cmp =
  testGroup
    name
    [ testCase "identity" $ assertBool "identity" $ (f mempty) `cmp` mempty,
      testProperty "monoid" $ prop_monoid gen f cmp
    ]

laxMonotoneMap ::
  (Show p, Monoid p, Monoid q, Preorder q) =>
  Gen p ->
  (p -> q) ->
  TestTree
laxMonotoneMap gen f = monoidalMap "lax" gen f lte

strongMonotoneMap ::
  (Show p, Monoid p, Monoid q, Preorder q) =>
  Gen p ->
  (p -> q) ->
  TestTree
strongMonotoneMap gen f = monoidalMap "strong" gen f (=~)

strictMonotoneMap ::
  (Show p, Monoid p, Monoid q, Eq q) =>
  Gen p ->
  (p -> q) ->
  TestTree
strictMonotoneMap gen f = monoidalMap "strict" gen f (==)
