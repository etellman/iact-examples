module Ch2.Sec2.Exercise28Test (tests) where

import Data.Monoid (All (All))
import Gen.Cost
import Monoid.BooleanMonoids
import Monoid.Cost
import Preorder.MonoidalMapProperties
import Test.Tasty

d :: CostPreorder Int -> PartialOrdAll
d (CostPreorder (Cost 0)) = PartialOrdAll $ All True
d _ = PartialOrdAll $ All False

u :: CostPreorder Int -> PartialOrdAll
u (CostPreorder Infinity) = PartialOrdAll $ All False
u _ = PartialOrdAll $ All True

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise28Test"
    [ testGroup
        "d"
        [ laxMonotoneMap genCostPreorder d,
          strongMonotoneMap genCostPreorder d,
          strictMonotoneMap genCostPreorder d
        ],
      testGroup
        "u"
        [ laxMonotoneMap genCostPreorder u,
          strongMonotoneMap genCostPreorder u,
          strictMonotoneMap genCostPreorder u
        ]
    ]
