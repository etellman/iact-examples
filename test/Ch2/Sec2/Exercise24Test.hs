module Ch2.Sec2.Exercise24Test (tests) where

import Ch2.MpoProperties
import Ch2.Sec2.CostMonoid
import Ch2.Sec2.GenCost
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog

genCostOpposite :: Gen CostOpPreorder
genCostOpposite = fmap CostOpPreorder genCost

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise24Test"
    [ monoid "monoid" genCostOpposite,
      testProperty "symmetry" $ prop_symmetry genCostOpposite,
      testProperty "monotonicity" $ prop_monotonicity genCostOpposite
    ]
