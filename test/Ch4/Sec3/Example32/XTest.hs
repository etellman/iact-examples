module Ch4.Sec3.Example32.XTest (tests) where

import Ch4.Sec3.Example32
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec3.Example32.XTest"
    [ testGroup
        "A"
        [ testCase "A" $ xDistance A A @?= Cost 0,
          testCase "B" $ xDistance A B @?= Cost 2
        ],
      testGroup
        "B"
        [ testCase "A" $ xDistance B A @?= Infinity,
          testCase "B" $ xDistance B B @?= Cost 0
        ]
    ]
