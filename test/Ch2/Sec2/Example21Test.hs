module Ch2.Sec2.Example21Test (tests) where

import Ch2.MpoProperties
import Ch2.Sec2.CostMonoid
import Ch2.Sec2.GenCost
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Example21Test"
    [ monoid "monoid" genCostPreorder,
      testProperty "symmetry" $ prop_symmetry genCostPreorder,
      testProperty "monotonicity" $ prop_monotonicity genCostPreorder
    ]
