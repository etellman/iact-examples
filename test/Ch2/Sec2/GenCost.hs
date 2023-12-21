module Ch2.Sec2.GenCost (genCost) where

import Ch2.Sec2.CostMonoid
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genCost :: Gen Cost
genCost = do
  cs <-
    fmap Cost
      <$> Gen.list
        (Range.singleton 5)
        (Gen.realFloat (Range.exponentialFloat 0 1e6))
  Gen.element (Infinity : cs)
