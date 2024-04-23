module Ch2.Sec2.Example12Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Monoid.BooleanMonoids
import Preorder.MonoidalPreorderProperties
import Test.Tasty
import Test.Tasty.Hedgehog
import Data.Monoid (All(..))

genBool :: Gen PartialOrdAll
genBool = PartialOrdAll . All <$> Gen.bool

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Example12Test"
    [ monoid "monoid" genBool,
      testProperty "symmetry" $ prop_symmetry genBool,
      testProperty "monotonicity" $ prop_monotonicity genBool
    ]
