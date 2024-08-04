module Ch4.Sec3.Example32.ZTest (tests) where

import Ch4.Sec3.Exercise15
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec3.Example32.ZTest"
    [ testGroup
        "P"
        [ testCase "P" $ zDistance P P @?= Cost 0,
          testCase "Q" $ zDistance P Q @?= Cost 2,
          testCase "R" $ zDistance P R @?= Cost 4,
          testCase "S" $ zDistance P S @?= Cost 5
        ],
      testGroup
        "Q"
        [ testCase "P" $ zDistance Q P @?= Cost 4,
          testCase "Q" $ zDistance Q Q @?= Cost 0,
          testCase "R" $ zDistance Q R @?= Cost 2,
          testCase "S" $ zDistance Q S @?= Cost 3
        ],
      testGroup
        "R"
        [ testCase "P" $ zDistance R P @?= Cost 2,
          testCase "Q" $ zDistance R Q @?= Cost 4,
          testCase "R" $ zDistance R R @?= Cost 0,
          testCase "S" $ zDistance R S @?= Cost 1
        ]
    ]
