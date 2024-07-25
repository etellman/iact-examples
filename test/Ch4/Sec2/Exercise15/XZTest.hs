module Ch4.Sec2.Exercise15.XZTest (tests) where

import Ch4.Sec2.Exercise15
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Exercise15.XZTest"
    [ testGroup
        "A"
        [ testCase "P" $ phiRho A P @?= Cost 22,
          testCase "Q" $ phiRho A Q @?= Cost 24,
          testCase "R" $ phiRho A R @?= Cost 20,
          testCase "S" $ phiRho A S @?= Cost 21
        ],
      testGroup
        "B"
        [ testCase "P" $ phiRho B P @?= Cost 16,
          testCase "Q" $ phiRho B Q @?= Cost 18,
          testCase "R" $ phiRho B R @?= Cost 14,
          testCase "S" $ phiRho B S @?= Cost 15
        ],
      testGroup
        "C"
        [ testCase "P" $ phiRho C P @?= Cost 19,
          testCase "Q" $ phiRho C Q @?= Cost 21,
          testCase "R" $ phiRho C R @?= Cost 17,
          testCase "S" $ phiRho C S @?= Cost 18
        ],
      testGroup
        "D"
        [ testCase "P" $ phiRho D P @?= Cost 11,
          testCase "Q" $ phiRho D Q @?= Cost 13,
          testCase "R" $ phiRho D R @?= Cost 9,
          testCase "S" $ phiRho D S @?= Cost 10
        ]
    ]
