module Ch2.Sec2.Exercise27Test (tests) where

import Preorder.MonoidalMapProperties
import Monoid.BooleanMonoids
import Monoid.Cost
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty

genBool :: Gen BooleanAnd
genBool = BooleanAnd <$> Gen.bool

boolToCost :: BooleanAnd -> CostPreorder
boolToCost (BooleanAnd False) = CostPreorder Infinity
boolToCost (BooleanAnd True) = CostPreorder $ Cost 0

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise27Test"
    [ testGroup
        "Boolean -> Cost"
        [ laxMonotoneMap genBool boolToCost,
          strongMonotoneMap genBool boolToCost,
          strictMonotoneMap genBool boolToCost
        ]
    ]
