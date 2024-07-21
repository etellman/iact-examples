module Preorder.QuantaleTest (tests) where

import Data.Char (ord)
import Data.Matrix
import Monoid.Cost
import Preorder.Quantale
import Test.Tasty
import Test.Tasty.HUnit

eq_2_18 :: Matrix (Cost Int)
eq_2_18 = fromLists [[0, 4, 3], [3, 0, Infinity], [Infinity, 4, 0]]

indexOf :: Char -> Int
indexOf v = ord v - ord 'x' + 1

distance_2_18 :: Char -> Char -> Cost Int
distance_2_18 = distanceFunc eq_2_18 indexOf indexOf

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
              expected =
                fromLists $
                  [ [Infinity, Infinity, Infinity],
                    [2, Infinity, 2],
                    [2, 2, 2]
                  ]
          quantMult x y @?= expected,
      --
      testGroup
        "Equation 2.18"
        [ testCase "all" $ do
            let expected =
                  fromLists $
                    [ [0, 4, 3],
                      [3, 0, 6],
                      [7, 4, 0]
                    ]
            distances eq_2_18 @?= expected,
          testCase "x -> y" $ distance_2_18 'x' 'y' @?= 4,
          testCase "z -> x" $ distance_2_18 'z' 'x' @?= 7,
          testCase "y -> z" $ distance_2_18 'y' 'z' @?= 6
        ],
      testCase "power" $
        do
          let x =
                fromLists $
                  [ [0, Infinity, 3, Infinity],
                    [3, 0, Infinity, 2],
                    [Infinity, 4, 0, Infinity],
                    [Infinity, 4, 1, 0]
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
