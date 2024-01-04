module Ch2.Sec3.Example31Test (tests) where

import Ch2.Sec3.Example31
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.Preorder as PO
import Test.Tasty
import Test.Tasty.Hedgehog

genEx31 :: Gen Ex31
genEx31 = Gen.element vertices

prop_identity ::
  (Show p, Monoid q, Preorder q) =>
  Gen p ->
  (p -> p -> q) ->
  Property
prop_identity gen hom = property $ do
  -- set up
  x <- forAll gen

  -- exercise and verify
  H.assert $ (hom x x) PO.<= mempty

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
  H.assert $ ((hom x y) <> (hom y z)) PO.<= (hom x z)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Example31Test"
    [ testProperty "identity" $ prop_identity genEx31 ex31ToBool,
      testProperty "monoid operation" $ prop_mplus genEx31 ex31ToBool
    ]
