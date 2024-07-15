module Ch2.Sec4.Example55Test (tests) where

import Ch2.Sec4.Example55
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec4.Example55Test"
    [ testGroup
        "X"
        [ testCase "A -> B" $ xDistance A B @?= Cost 2,
          testCase "A -> C" $ xDistance A C @?= Cost 5,
          testCase "B -> C" $ xDistance B C @?= Cost 3,
          testCase "B -> A" $ xDistance B A @?= Infinity,
          testCase "C -> A" $ xDistance C A @?= Infinity
        ],
      testGroup
        "Y"
        [ testCase "P -> Q" $ yDistance P Q @?= Cost 5,
          testCase "Q -> P" $ yDistance Q P @?= Cost 8
        ],
      testGroup
        "product"
        [ testCase "(B, P) -> (C, Q)" $ xyDistance (B, P) (C, Q) @?= Cost 8,
          testCase "(B, P) -> (A, Q)" $ xyDistance (B, P) (A, Q) @?= Infinity,
          testCase "(A, P) -> (C, Q)" $ xyDistance (A, P) (C, Q) @?= Cost 10,
          testCase "(A, Q) -> (C, Q)" $ xyDistance (A, Q) (C, Q) @?= Cost 5
        ]
    ]
