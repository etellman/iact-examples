module Ch4.Sec2.Exercise10Test (tests) where

import Ch4.Sec2.Example9
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Exercise10Test"
    [ testGroup
        "A"
        [ testCase "x" $ cost A B X X @?= Cost 17,
          testCase "y" $ cost A D Y Y @?= Cost 20,
          testCase "z" $ cost A B X Z @?= Cost 20
        ],
      testGroup
        "B"
        [ testCase "x" $ cost B B X X @?= Cost 11,
          testCase "y" $ cost B D Y Y @?= Cost 14,
          testCase "z" $ cost B B X Z @?= Cost 14
        ],
      testGroup
        "C"
        [ testCase "x" $ cost C B X X @?= Cost 14,
          testCase "y" $ cost C D Y Y @?= Cost 17,
          testCase "z" $ cost C B X Z @?= Cost 17
        ],
      testGroup
        "D"
        [ testCase "x" $ cost D D Y X @?= Cost 12,
          testCase "y" $ cost D D Y Y @?= Cost 9,
          testCase "z" $ cost D D Y Z @?= Cost 15
        ]
    ]
