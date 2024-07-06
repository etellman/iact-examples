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

genCost :: Gen (Cost Int)
genCost = do
  cs <- Gen.list
        (Range.singleton 5)
        (genFiniteCost $ Range.linear 0 1000)
  Gen.element (Infinity : cs)

genFiniteCost :: Range Int -> Gen (Cost Int)
genFiniteCost range = do
  fmap Cost (Gen.int range)

genCostPreorder :: Gen (CostPreorder Int)
genCostPreorder = fmap CostPreorder genCost

genCostOpposite :: Gen (CostOpPreorder Int)
genCostOpposite = fmap CostOpPreorder genCost
