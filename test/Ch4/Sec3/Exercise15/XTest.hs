module Ch4.Sec3.Exercise15.XTest (tests) where

import Ch4.Sec3.Exercise15
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec3.Exercise15.XTest"
    [ testGroup
        "A"
        [ testCase "A" $ xDistance A A @?= Cost 0,
          testCase "B" $ xDistance A B @?= Cost 6,
          testCase "C" $ xDistance A C @?= Cost 3,
          testCase "D" $ xDistance A D @?= Cost 11
        ],
      testGroup
        "B"
        [ testCase "A" $ xDistance B A @?= Cost 2,
          testCase "B" $ xDistance B B @?= Cost 0,
          testCase "C" $ xDistance B C @?= Cost 5,
          testCase "D" $ xDistance B D @?= Cost 5
        ],
      testGroup
        "C"
        [ testCase "A" $ xDistance C A @?= Cost 5,
          testCase "B" $ xDistance C B @?= Cost 3,
          testCase "C" $ xDistance C C @?= Cost 0,
          testCase "D" $ xDistance C D @?= Cost 8
        ],
      testGroup
        "D"
        [ testCase "A" $ xDistance D A @?= Cost 9,
          testCase "B" $ xDistance D B @?= Cost 7,
          testCase "C" $ xDistance D C @?= Cost 4,
          testCase "D" $ xDistance D D @?= Cost 0
        ]
    ]
