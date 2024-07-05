module Ch2.Sec4.Example45 (f, cf) where

import Monoid.BooleanMonoids
import Monoid.Cost (Cost (..), CostPreorder (..))
import Data.Monoid (All(All))

f :: (Eq a, Num a) => CostPreorder a -> PartialOrdAll
f (CostPreorder (Cost 0)) = PartialOrdAll $ All True
f _ = PartialOrdAll $ All False

cf :: (Eq a, Num a) => CostPreorder a -> CostPreorder a -> PartialOrdAll
cf c d = f (c <> d)
