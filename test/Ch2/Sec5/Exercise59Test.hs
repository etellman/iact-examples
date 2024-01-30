module Ch2.Sec5.Exercise59Test (tests) where

import Gen.Cost (genCostPreorder)
import qualified Monoid.Cost as C
import Preorder.MonoidalClosedProperties (testClosed)
import Test.Tasty

testCostClosed :: String -> C.Cost -> TestTree
testCostClosed name c =
  let (C.CostPreorder x) -* (C.CostPreorder y) = C.CostPreorder (x C.-* y)
      join xs =  Just $ foldr max (C.CostPreorder C.Infinity) xs
   in testClosed name genCostPreorder (-*) join (C.CostPreorder c)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec5.Exercise59Test"
    [ testCostClosed "zero" (C.Cost 0),
      testCostClosed "non-zero finite" (C.Cost 17),
      testCostClosed "infinite" C.Infinity
    ]
