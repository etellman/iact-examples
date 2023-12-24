module Ch2.Sec2.Example3Test (tests) where

import Ch2.Sec2.MonoidalPreorderProperties
import Ch2.Sec2.RealMonoids
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genRealPlus :: Gen RealPlus
genRealPlus = RealPlus <$> Gen.realFloat (Range.exponentialFloat (-1000) 1000)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Example3Test"
    [ monoid "monoid" genRealPlus,
      testProperty "monotonicity" $ prop_monotonicity genRealPlus,
      testProperty "symmetry" $ prop_symmetry genRealPlus
    ]
