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
          quantMult x y @?= z,
      --
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
          distances x @?= z,
      --
      testCase "power" $
        do
          let x =
                fromLists $
                  [ [0,        Infinity,    3,        Infinity],
                    [3,        0,           Infinity, 2],
                    [Infinity, 4,           0,        Infinity],
                    [Infinity, 4,           1,        0]
                  ] ::
                  Matrix (Cost Int)
              x2 =
                fromLists $
                [ [0, 7, 3, Infinity],
                  [3, 0, 3, 2],
                  [7, 4, 0, 6],
                  [7, 4, 1, 0]
                ]
              x3 =
                fromLists $
                [ [0, 7, 3, 9],
                  [3, 0, 3, 2],
                  [7, 4, 0, 6],
                  [7, 4, 1, 0]
                ]
          quantPower x 2 @?= x2
          quantPower x 3 @?= x3
    ]
