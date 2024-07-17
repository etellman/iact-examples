module Ch4.Sec2.Exercise8Test (tests) where

import Ch4.Sec2.Example7
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Exercise8Test"
    [ testGroup
        "South"
        [ testCase "A" $ assertBool "reachable" $ reachable South A,
          testCase "B" $ assertBool "reachable" $ reachable South B,
          testCase "C" $ assertBool "reachable" $ reachable South C,
          testCase "D" $ assertBool "reachable" $ reachable South D,
          testCase "E" $ assertBool "reachable" $ reachable South E
        ],
      testGroup
        "East"
        [ testCase "A" $ assertBool "reachable" $ reachable East A,
          testCase "B" $ assertBool "reachable" $ reachable East B,
          testCase "C" $ assertBool "reachable" $ reachable East C,
          testCase "D" $ assertBool "reachable" $ reachable East D,
          testCase "E" $ assertBool "reachable" $ reachable East E
        ],
      testGroup
        "North"
        [ testCase "A" $ assertBool "reachable" $ reachable North A,
          testCase "not B" $ assertBool "not reachable" $ not $ reachable North B,
          testCase "C" $ assertBool "reachable" $ reachable North C,
          testCase "not D" $ assertBool "not reachable" $ not $ reachable North D,
          testCase "E" $ assertBool "reachable" $ reachable North E
        ],
      testGroup
        "West"
        [ testCase "A" $ assertBool "reachable" $ reachable West A,
          testCase "not B" $ assertBool "not reachable" $ not $ reachable West B,
          testCase "C" $ assertBool "reachable" $ reachable West C,
          testCase "not D" $ assertBool "not reachable" $ not $ reachable West D,
          testCase "E" $ assertBool "reachable" $ reachable West E
        ]
    ]
