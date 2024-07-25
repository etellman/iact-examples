module Ch4.Sec2.Exercise15.XYTest (tests) where

import Ch4.Sec2.Exercise15
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Exercise15.XYTest"
    [ testGroup
        "A"
        [ testCase "X" $ phi A X @?= Cost 17,
          testCase "Y" $ phi A Y @?= Cost 20,
          testCase "Z" $ phi A Z @?= Cost 20
        ],
      testGroup
        "B"
        [ testCase "X" $ phi B X @?= Cost 11,
          testCase "Y" $ phi B Y @?= Cost 14,
          testCase "Z" $ phi B Z @?= Cost 14
        ],
      testGroup
        "C"
        [ testCase "X" $ phi C X @?= Cost 14,
          testCase "Y" $ phi C Y @?= Cost 17,
          testCase "Z" $ phi C Z @?= Cost 17
        ],
      testGroup
        "C"
        [ testCase "X" $ phi D X @?= Cost 12,
          testCase "Y" $ phi D Y @?= Cost 9,
          testCase "Z" $ phi D Z @?= Cost 15
        ]
    ]
