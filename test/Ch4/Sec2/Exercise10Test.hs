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
        [ testCase "x" $ cost A B X X @?= IntCost (Cost 17),
          testCase "y" $ cost A D Y Y @?= IntCost (Cost 20),
          testCase "z" $ cost A B X Z @?= IntCost (Cost 20)
        ],
      testGroup
        "B"
        [ testCase "x" $ cost B B X X @?= IntCost (Cost 11),
          testCase "y" $ cost B D Y Y @?= IntCost (Cost 14),
          testCase "z" $ cost B B X Z @?= IntCost (Cost 14)
        ],
      testGroup
        "C"
        [ testCase "x" $ cost C B X X @?= IntCost (Cost 14),
          testCase "y" $ cost C D Y Y @?= IntCost (Cost 17),
          testCase "z" $ cost C B X Z @?= IntCost (Cost 17)
        ],
      testGroup
        "D"
        [ testCase "x" $ cost D D Y X @?= IntCost (Cost 12),
          testCase "y" $ cost D D Y Y @?= IntCost (Cost 9),
          testCase "z" $ cost D D Y Z @?= IntCost (Cost 15)
        ]
    ]
