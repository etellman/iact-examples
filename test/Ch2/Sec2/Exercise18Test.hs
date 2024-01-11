module Ch2.Sec2.Exercise18Test (tests) where

import Preorder.MonoidalPreorderProperties
import Ch2.Sec2.YesNoMaybe
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

genYnmMin :: Gen YnmMin
genYnmMin = YnmMin <$> Gen.element [No, Maybe, Yes]

genYnmMax :: Gen YnmMax
genYnmMax = YnmMax <$> Gen.element [No, Maybe, Yes]

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise18Test"
    [ testGroup
        "min"
        [ monoid "monoid" genYnmMin,
          testProperty "symmetry" $ prop_symmetry genYnmMin,
          testProperty "monotonicity" $ prop_monotonicity genYnmMin
        ],
      testGroup
        "max"
        [ monoid "monoid" genYnmMax,
          testProperty "symmetry" $ prop_symmetry genYnmMax,
          testProperty "monotonicity" $ prop_monotonicity genYnmMax
        ]
    ]
