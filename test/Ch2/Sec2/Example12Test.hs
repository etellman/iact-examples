module Ch2.Sec2.Example12Test (tests) where

import Ch2.MpoProperties
import Ch2.Sec2.BooleanMonoids
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

genBool :: Gen BooleanAnd
genBool = BooleanAnd <$> Gen.bool

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Example12Test"
    [ monoid "monoid" genBool,
      testProperty "symmetry" $ prop_symmetry genBool,
      testProperty "monotonicity" $ prop_monotonicity genBool
    ]
