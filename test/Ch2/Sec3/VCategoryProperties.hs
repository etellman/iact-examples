module Ch2.Sec3.VCategoryProperties (tests) where

import Hedgehog as H
import Preorder.Preorder as PO
import Test.Tasty
import Test.Tasty.Hedgehog

prop_identity ::
  (Show p, Monoid q, Preorder q) =>
  Gen p ->
  (p -> p -> q) ->
  Property
prop_identity gen hom = property $ do
  -- set up
  x <- forAll gen

  -- exercise and verify
  H.assert $ hom x x PO.<= mempty

prop_mplus ::
  (Show p, Monoid q, Preorder q) =>
  Gen p ->
  (p -> p -> q) ->
  Property
prop_mplus gen hom = property $ do
  -- set up
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  -- exercise and verify
  H.assert $ (hom x y <> hom y z) PO.<= hom x z

tests ::
  (Show p, Monoid q, Preorder q) =>
  Gen p ->
  (p -> p -> q) ->
  TestTree
tests gen hom =
  testGroup
    "V-Category"
    [ testProperty "identity" $ prop_identity gen hom,
      testProperty "monoid operation" $ prop_mplus gen hom
    ]
