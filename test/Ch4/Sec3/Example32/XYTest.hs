module Ch4.Sec3.Example32.XYTest (tests) where

import Ch4.Sec3.Example32
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec3.Example32.XYTest"
    [ testGroup
        "A"
        [ testCase "X" $ phi A X @?= Cost 5,
          testCase "Y" $ phi A Y @?= Cost 8
        ],
      testGroup
        "B"
        [ testCase "X" $ phi B X @?= Infinity,
          testCase "Y" $ phi B Y @?= Infinity
        ]
    ]
