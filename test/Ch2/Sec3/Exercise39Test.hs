module Ch2.Sec3.Exercise39Test (tests) where

import Ch2.Sec3.Figure18
import Data.Matrix
import Preorder.Quantale
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Exercise39Test"
    [ testCase "all" $ do
        let expected =
              fromLists
                [ [0, 6, 3, 11],
                  [2, 0, 5, 5],
                  [5, 3, 0, 8],
                  [11, 9, 6, 0]
                ]
        distances xWeights @?= expected,
      testCase "A -> D" $ distanceX 'A' 'D' @?= 11,
      testCase "D -> B" $ distanceX 'D' 'B' @?= 9
    ]
