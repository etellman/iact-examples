module Ch2.Sec2.Exercise24Test (tests) where

import Preorder.MonoidalPreorderProperties
import Gen.Cost
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise24Test"
    [ monoid "monoid" genCostOpposite,
      testProperty "symmetry" $ prop_symmetry genCostOpposite,
      testProperty "monotonicity" $ prop_monotonicity genCostOpposite
    ]
