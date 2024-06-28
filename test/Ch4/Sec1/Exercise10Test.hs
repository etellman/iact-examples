module Ch4.Sec1.Exercise10Test (tests) where

import Ch4.Sec1.Example9
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec1.Exercise10Test"
    [ testGroup
        "A"
        [ testCase "x" $ cost A B X X @?= 17,
          testCase "y" $ cost A D Y Y @?= 20,
          testCase "z" $ cost A B X Z @?= 20
        ],
      testGroup
        "B"
        [ testCase "x" $ cost B B X X @?= 11,
          testCase "y" $ cost B D Y Y @?= 14,
          testCase "z" $ cost B B X Z @?= 14
        ],
      testGroup
        "C"
        [ testCase "x" $ cost C B X X @?= 14,
          testCase "y" $ cost C D Y Y @?= 17,
          testCase "z" $ cost C B X Z @?= 17
        ],
      testGroup
        "D"
        [ testCase "x" $ cost D D Y X @?= 12,
          testCase "y" $ cost D D Y Y @?= 9,
          testCase "z" $ cost D D Y Z @?= 15
        ]
    ]
