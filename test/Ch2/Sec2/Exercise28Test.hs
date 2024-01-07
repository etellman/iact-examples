module Ch2.Sec2.Exercise28Test (tests) where

import Ch2.Sec2.MonoidalMapProperties
import Monoid.BooleanMonoids
import Monoid.Cost
import Gen.Cost
import Test.Tasty

d :: CostPreorder -> BooleanAnd
d (CostPreorder (Cost 0)) = BooleanAnd True
d _ = BooleanAnd False

u :: CostPreorder -> BooleanAnd
u (CostPreorder Infinity) = BooleanAnd False
u _ = BooleanAnd True

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
