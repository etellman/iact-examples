module Ch2.Sec2.Exercise15Test (tests) where

import Preorder.MonoidalPreorderProperties
import Monoid.NaturalMonoids
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genNatural :: Gen NaturalTimes
genNatural = NaturalTimes <$> Gen.int (Range.linear 1 1000)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise15Test"
    [ monoid "monoid" genNatural,
      testProperty "symmetry" $ prop_symmetry genNatural,
      testProperty "monotonicity" $ prop_monotonicity genNatural
    ]
