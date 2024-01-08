module Ch2.Sec4.Example55Test (tests) where

import Ch2.Sec4.Example55
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec4.Example55Test"
    [ testCase "(A, P)" $
        xyarrows (XYVertex (A, P))
          @?= [ XYArrow (XYVertex (A, P)) (XYVertex (B, P)) 2,
                XYArrow (XYVertex (A, P)) (XYVertex (A, Q)) 5
              ],
      testCase "(B, P)" $
        xyarrows (XYVertex (B, Q))
          @?= [ XYArrow (XYVertex (B, Q)) (XYVertex (C, Q)) 3,
                XYArrow (XYVertex (B, Q)) (XYVertex (B, P)) 8
              ]
    ]
