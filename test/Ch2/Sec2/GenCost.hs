module Ch2.Sec2.GenCost
  ( genCost,
    genCostPreorder,
    genCostOpposite,
  )
where

import Monoid.Cost
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

genCostPreorder :: Gen CostPreorder
genCostPreorder = fmap CostPreorder genCost

genCostOpposite :: Gen CostOpPreorder
genCostOpposite = fmap CostOpPreorder genCost
