module Ch2.Sec4.Example45 (f, cf) where

import Monoid.BooleanMonoids
import Monoid.Cost (Cost (..), CostPreorder (..))
import Data.Monoid (All(All))

f :: CostPreorder -> PartialOrdAll
f (CostPreorder (Cost 0)) = PartialOrdAll $ All True
f _ = PartialOrdAll $ All False

cf :: CostPreorder -> CostPreorder -> PartialOrdAll
cf c d = f (c <> d)
