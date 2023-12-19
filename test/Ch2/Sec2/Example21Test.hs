module Ch2.Sec2.Example21Test (tests) where

import Ch2.MpoProperties
import Ch2.Sec2.CostMonoid
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genCost :: Gen Cost
genCost = do
  cs <-
    fmap Cost
      <$> Gen.list
        (Range.singleton 5)
        (Gen.realFloat (Range.exponentialFloat 0 1e6))
  Gen.element (Infinity : cs)

genCostPO :: Gen CostPO
genCostPO = fmap CostPO genCost

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Example21Test"
    [ monoid "monoid" genCostPO,
      testProperty "symmetry" $ prop_symmetry genCostPO,
      testProperty "monotonicity" $ prop_monotonicity genCostPO
    ]
