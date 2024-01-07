module Ch2.Sec2.Exercise13Test (tests) where

import Ch2.Sec2.MonoidalPreorderProperties
import Monoid.BooleanMonoids
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

genBool :: Gen BooleanOr
genBool = BooleanOr <$> Gen.bool

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise13Test"
    [ monoid "monoid" genBool,
      testProperty "symmetry" $ prop_symmetry genBool,
      testProperty "monotonicity" $ prop_monotonicity genBool
    ]
