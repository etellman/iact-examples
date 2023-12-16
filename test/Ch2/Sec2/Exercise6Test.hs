module Ch2.Sec2.Exercise6Test (tests) where

import Ch2.MpoProperties
import Ch2.Sec2.RealMonoids
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genReal :: Gen RealDiscrete
genReal = RealDiscrete <$> Gen.realFloat (Range.exponentialFloat (-1000) 1000)

-- | only a few choices so the monotonicity test will find elements equal to each other
genRealNarrow :: Gen RealDiscrete
genRealNarrow = Gen.element $ map RealDiscrete [0, 1]

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise6Test"
    [ monoid "monoid" genReal,
      testProperty "symmetry" $ prop_symmetry genReal,
      testProperty "monotonicity only non-negative" $ prop_monotonicity genRealNarrow
    ]
