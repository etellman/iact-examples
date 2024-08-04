module Ch4.Sec3.Exercise15.XZTest (tests) where

import Ch4.Sec3.Exercise15
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec3.Exercise15.XZTest"
    [ testGroup
        "A"
        [ testCase "P" $ phiPsi A P @?= Cost 22,
          testCase "Q" $ phiPsi A Q @?= Cost 24,
          testCase "R" $ phiPsi A R @?= Cost 20,
          testCase "S" $ phiPsi A S @?= Cost 21
        ],
      testGroup
        "B"
        [ testCase "P" $ phiPsi B P @?= Cost 16,
          testCase "Q" $ phiPsi B Q @?= Cost 18,
          testCase "R" $ phiPsi B R @?= Cost 14,
          testCase "S" $ phiPsi B S @?= Cost 15
        ],
      testGroup
        "C"
        [ testCase "P" $ phiPsi C P @?= Cost 19,
          testCase "Q" $ phiPsi C Q @?= Cost 21,
          testCase "R" $ phiPsi C R @?= Cost 17,
          testCase "S" $ phiPsi C S @?= Cost 18
        ],
      testGroup
        "D"
        [ testCase "P" $ phiPsi D P @?= Cost 11,
          testCase "Q" $ phiPsi D Q @?= Cost 13,
          testCase "R" $ phiPsi D R @?= Cost 9,
          testCase "S" $ phiPsi D S @?= Cost 10
        ]
    ]
