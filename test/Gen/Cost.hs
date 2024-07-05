module Gen.Cost
  ( genCost,
    genFiniteCost,
    genCostPreorder,
    genCostOpposite,
  )
where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Monoid.Cost

genCost :: Gen (Cost ApproximateDouble)
genCost = do
  cs <-
    fmap Cost
      <$> Gen.list
        (Range.singleton 5)
        (Gen.realFloat (Range.exponentialFloat 0 1e6))
  Gen.element (Infinity : cs)

genFiniteCost :: ApproximateDouble -> Gen (Cost ApproximateDouble)
genFiniteCost from = do
  fmap Cost (Gen.realFloat $ Range.exponentialFloat from 1e6)

genCostPreorder :: Gen (CostPreorder ApproximateDouble)
genCostPreorder = fmap CostPreorder genCost

genCostOpposite :: Gen (CostOpPreorder ApproximateDouble)
genCostOpposite = fmap CostOpPreorder genCost
