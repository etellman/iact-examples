module Preorder.QuantaleTest (tests) where

import Data.Matrix
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit
import Preorder.Quantale

tests :: TestTree
tests =
  testGroup
    "Preorder.MatrixTest"
    [ testCase "multiply" $
        do
          let x =
                fromLists $
                  (fmap . fmap)
                    EdgeCost
                    [ [Infinity, Infinity],
                      [Infinity, 1],
                      [1, 1]
                    ]
              y =
                fromLists $
                  (fmap . fmap)
                    EdgeCost
                    [ [1, 1, Infinity],
                      [1, Infinity, 1]
                    ]
              z =
                fromLists $
                  (fmap . fmap)
                    EdgeCost
                    [ [Infinity, Infinity, Infinity],
                      [2, Infinity, 2],
                      [2, 2, 2]
                    ]
          multStd x y @?= z
    ]
