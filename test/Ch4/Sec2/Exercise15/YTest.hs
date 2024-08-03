module Ch4.Sec2.Exercise15.YTest (tests) where

import Ch4.Sec2.Exercise15
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Exercise15.YTest"
    [ testGroup
        "X"
        [ testCase "X" $ yDistance X X @?= Cost 0,
          testCase "Y" $ yDistance X Y @?= Cost 4,
          testCase "Z" $ yDistance X Z @?= Cost 3
        ],
      testGroup
        "Y"
        [ testCase "X" $ yDistance Y X @?= Cost 3,
          testCase "Y" $ yDistance Y Y @?= Cost 0,
          testCase "Z" $ yDistance Y Z @?= Cost 6
        ],
      testGroup
        "Z"
        [ testCase "X" $ yDistance Z X @?= Cost 7,
          testCase "Y" $ yDistance Z Y @?= Cost 4,
          testCase "Y" $ yDistance Z Z @?= Cost 0
        ]
    ]
