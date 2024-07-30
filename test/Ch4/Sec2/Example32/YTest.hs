module Ch4.Sec2.Example32.YTest (tests) where

import Ch4.Sec2.Example32
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Example32.YTest"
    [ testGroup
        "X"
        [ testCase "X" $ yDistance X X @?= Cost 0,
          testCase "Y" $ yDistance X Y @?= Cost 3
        ],
      testGroup
        "Y"
        [ testCase "X" $ yDistance Y X @?= Cost 4,
          testCase "Y" $ yDistance Y Y @?= Cost 0
        ]
    ]
