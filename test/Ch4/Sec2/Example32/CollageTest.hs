module Ch4.Sec2.Example32.CollageTest (tests) where

import Ch4.Sec2.Example32
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Example32.CollageTest"
    [ testGroup
        "A"
        [ testCase "A" $ col (Left A) (Left A) @?= Cost 0,
          testCase "B" $ col (Left A) (Left B) @?= Cost 2,
          testCase "X" $ col (Left A) (Right X) @?= Cost 5,
          testCase "Y" $ col (Left A) (Right Y) @?= Cost 8
        ],
      testGroup
        "B"
        [ testCase "A" $ col (Left B) (Left A) @?= Infinity,
          testCase "B" $ col (Left B) (Left B) @?= Cost 0,
          testCase "X" $ col (Left B) (Right X) @?= Infinity,
          testCase "Y" $ col (Left B) (Right Y) @?= Infinity
        ],
      testGroup
        "X"
        [ testCase "A" $ col (Right X) (Left A) @?= Infinity,
          testCase "B" $ col (Right X) (Left B) @?= Infinity,
          testCase "X" $ col (Right X) (Right X) @?= Cost 0,
          testCase "Y" $ col (Right X) (Right Y) @?= Cost 3
        ],
      testGroup
        "Y"
        [ testCase "A" $ col (Right Y) (Left A) @?= Infinity,
          testCase "B" $ col (Right Y) (Left B) @?= Infinity,
          testCase "X" $ col (Right Y) (Right X) @?= Cost 4,
          testCase "Y" $ col (Right Y) (Right Y) @?= Cost 0
        ]
    ]
