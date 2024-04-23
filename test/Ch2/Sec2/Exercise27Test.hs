module Ch2.Sec2.Exercise27Test (tests) where

import Data.Monoid (All (All))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Monoid.Cost
import Preorder.MonoidalMapProperties
import Test.Tasty

genBool :: Gen All
genBool = All <$> Gen.bool

boolToCost :: All -> CostPreorder
boolToCost (All False) = CostPreorder Infinity
boolToCost (All True) = CostPreorder $ Cost 0

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
