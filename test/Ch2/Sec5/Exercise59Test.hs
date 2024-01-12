module Ch2.Sec5.Exercise59Test (tests) where

import Gen.Cost (genCostPreorder)
import Monoid.Cost
import Preorder.MonoidalMapProperties
import Test.Tasty

testClosed :: String -> Cost -> TestTree
testClosed name c =
  let rightAdjunct (CostPreorder x) = CostPreorder (x -* c)
   in testGroup
        name
        [ namedLaxMonotoneMap "left" genCostPreorder ((CostPreorder c) <>),
          namedLaxMonotoneMap "right" genCostPreorder rightAdjunct
        ]

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec5.Exercise59Test"
    [ testClosed "zero" (Cost 0),
      testClosed "non-zero finite" (Cost 17),
      testClosed "infinite" Infinity
    ]
