module Ch2.Sec4.Example45Test (tests) where

import Preorder.MonoidalMapProperties
import Properties.VCategoryProperties (vCategoryTests)
import Ch2.Sec4.Example45
import Gen.Cost (genCostPreorder)
import Hedgehog as H
import Preorder.Preorder as PO
import Test.Tasty
import Test.Tasty.Hedgehog

prop_cf :: Property
prop_cf = property $ do
  -- set up
  c <- forAll genCostPreorder
  d <- forAll genCostPreorder

  -- exercise and verify
  H.assert $ cf c d PO.<= (f c <> f d)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec4.Example45Test"
    [ testGroup "f montone map" [strictMonotoneMap genCostPreorder f],
      testProperty "cf" prop_cf,
      vCategoryTests "V-Category" genCostPreorder cf
    ]
