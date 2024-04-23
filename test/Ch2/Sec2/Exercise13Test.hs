module Ch2.Sec2.Exercise13Test (tests) where

import Data.Monoid (Any (Any))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Monoid.BooleanMonoids (PartialOrdAny (..))
import Preorder.MonoidalPreorderProperties
import Test.Tasty
import Test.Tasty.Hedgehog

genBool :: Gen PartialOrdAny
genBool = PartialOrdAny . Any <$> Gen.bool

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise13Test"
    [ monoid "monoid" genBool,
      testProperty "symmetry" $ prop_symmetry genBool,
      testProperty "monotonicity" $ prop_monotonicity genBool
    ]
