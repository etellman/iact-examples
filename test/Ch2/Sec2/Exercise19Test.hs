module Ch2.Sec2.Exercise19Test (tests) where

import Preorder.MonoidalPreorderProperties
import Monoid.CharSetMonoid
import Data.Set (toList, fromList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

genCharSet :: Gen CharSetMonoid
genCharSet = CharSetMonoid . toList <$> Gen.subset (fromList chars)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise19Test"
    [ monoid "monoid" genCharSet,
      testProperty "symmetry" $ prop_symmetry genCharSet,
      testProperty "monotonicity" $ prop_monotonicity genCharSet
    ]
