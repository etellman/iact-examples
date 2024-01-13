module Ch2.Sec5.Exercise59Test (tests) where

import Gen.Cost (genCostPreorder)
import Monoid.Cost
import Preorder.MonoidalClosedProperties (testClosed)
import Test.Tasty

testCostClosed :: String -> Cost -> TestTree
testCostClosed name c =
  let rightAdjunct (CostPreorder x) = CostPreorder (x -* c)
      leftAdjunct = ((CostPreorder c) <>)
   in testClosed name genCostPreorder leftAdjunct rightAdjunct

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec5.Exercise59Test"
    [ testCostClosed "zero" (Cost 0),
      testCostClosed "non-zero finite" (Cost 17),
      testCostClosed "infinite" Infinity
    ]
