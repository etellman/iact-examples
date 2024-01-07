module Ch2.Sec4.Example45 (f, cf) where

import Monoid.BooleanMonoids
import Monoid.Cost (Cost (..), CostPreorder (..))

f :: CostPreorder -> BooleanAnd
f (CostPreorder (Cost 0)) = BooleanAnd True
f _ = BooleanAnd False

cf :: CostPreorder -> CostPreorder -> BooleanAnd
cf c d = f (c <> d)
