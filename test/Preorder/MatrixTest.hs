module Preorder.MatrixTest (tests) where

import Data.Matrix
import Numeric.Natural
import Test.Tasty
import Test.Tasty.HUnit

multPower :: Num a => Matrix a -> Natural -> Matrix a
multPower x 1 = x
multPower x n = multPower (multStd x x) (n - 1)

tests :: TestTree
tests =
  testGroup
    "Preorder.MatrixTest"
    [ testCase "multiplication" $
        do
          let x = matrix 3 3 $ \(i, _) -> i
              y = matrix 3 3 $ \(_, j) -> j

          multStd x y
            @?= fromLists
              [ [3, 6, 9],
                [6, 12, 18],
                [9, 18, 27]
              ],
      testGroup
        "power"
        [ testCase
            "one"
            $ do
              let x = matrix 3 3 $ \(i, _) -> i

              multPower x 1 @?= x,
          testCase
            "cube"
            $ do
              let x = matrix 3 3 $ \(i, _) -> i

              multPower x 3
                @?= fromLists
                  [ [216, 216, 216],
                    [432, 432, 432],
                    [648, 648, 648]
                  ]
        ]
    ]
