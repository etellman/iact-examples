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

genCost :: Range Int -> Gen (Cost Int)
genCost range = do
  cs <- Gen.list
        (Range.singleton 5)
        (genFiniteCost range)
  Gen.element (Infinity : cs)

genFiniteCost :: Range Int -> Gen (Cost Int)
genFiniteCost range = do
  fmap Cost (Gen.int range)

genCostPreorder :: Gen (CostPreorder Int)
genCostPreorder = fmap CostPreorder $ genCost (Range.linear 0 1000)

genCostOpposite :: Gen (CostOpPreorder Int)
genCostOpposite = fmap CostOpPreorder $ genCost (Range.linear 0 1000)
