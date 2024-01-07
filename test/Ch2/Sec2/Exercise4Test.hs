module Ch2.Sec2.Exercise4Test (tests) where

import Ch2.Sec2.MonoidalPreorderProperties
import Monoid.RealMonoids
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.ApproximateDouble (ApproximateDouble)
import Test.Tasty
import Test.Tasty.Hedgehog

genReal :: ApproximateDouble -> Gen RealTimes
genReal from = RealTimes <$> Gen.realFloat (Range.exponentialFloat from 1000)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise4Test"
    [ testGroup
        "all reals"
        [ monoid "monoid" $ genReal (-1000),
          testProperty "symmetry" $ prop_symmetry $ genReal (-1000)
        ],
      testProperty "monotonicity only non-negative" $ prop_monotonicity $ genReal 0
    ]
