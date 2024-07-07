module Preorder.QuantaleTest (tests) where

import Data.Matrix
import Monoid.Cost
import Preorder.Quantale
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Preorder.QuantaleTest"
    [ testCase "Example 2.76" $
        do
          let x =
                fromLists $
                  [ [Infinity, Infinity],
                    [Infinity, 1 :: Cost Int],
                    [1, 1]
                  ]
              y =
                fromLists $
                  [ [1, 1, Infinity],
                    [1, Infinity, 1]
                  ]
              z =
                fromLists $
                  [ [Infinity, Infinity, Infinity],
                    [2, Infinity, 2],
                    [2, 2, 2]
                  ]
          multStd3 x y @?= z,
      testCase "Equation 2.18" $
        do
          let x =
                fromLists $
                  [ [0, 4, (3 :: Cost Int)],
                    [3, 0, Infinity],
                    [Infinity, 4, 0]
                  ]
              z =
                fromLists $
                  [ [0, 4, 3],
                    [3, 0, 6],
                    [7, 4, 0]
                  ]
          distances x @?= z
    ]
