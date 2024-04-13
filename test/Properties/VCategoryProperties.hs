module Properties.VCategoryProperties (vCategoryTests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Preorder.Preorder as PO
import Test.Tasty
import Test.Tasty.Hedgehog

prop_identity ::
  (Show p, Preorder q, Monoid q) =>
  Gen p ->
  -- | hom
  (p -> p -> q) ->
  Property
prop_identity gen h = property $ do
  -- set up
  x <- forAll gen

  -- exercise and verify
  H.assert $ h x x PO.<= mempty

prop_composition ::
  (Show p, Monoid q, Preorder q) =>
  Gen p ->
  -- | hom
  (p -> p -> q) ->
  Property
prop_composition gen h = property $ do
  -- set up
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  -- exercise and verify
  H.assert $ (h x y <> h y z) PO.<= h x z

vCategoryTests ::
  (Show p, Monoid q, Preorder q) =>
  String ->
  Gen p ->
  -- | hom
  (p -> p -> q) ->
  TestTree
vCategoryTests name gen h =
  testGroup
    name
    [ testProperty "identity" $ prop_identity gen h,
      testProperty "composition" $ prop_composition gen h
    ]
