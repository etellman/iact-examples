module Ch4.Sec4.Exercise39Test (tests) where

import Ch4.Sec3.Exercise33
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec3.Exercise39Test"
    [ testGroup
        "A"
        [ testCase "A" $ col (Left A) (Left A) @?= Cost 0,
          testCase "B" $ col (Left A) (Left B) @?= Cost 6,
          testCase "C" $ col (Left A) (Left C) @?= Cost 3,
          testCase "D" $ col (Left A) (Left D) @?= Cost 11,
          testCase "X" $ col (Left A) (Right X) @?= Cost 17,
          testCase "Y" $ col (Left A) (Right Y) @?= Cost 20,
          testCase "Z" $ col (Left A) (Right Z) @?= Cost 20
        ],
      testGroup
        "B"
        [ testCase "A" $ col (Left B) (Left A) @?= Cost 2,
          testCase "B" $ col (Left B) (Left B) @?= Cost 0,
          testCase "C" $ col (Left B) (Left C) @?= Cost 5,
          testCase "D" $ col (Left B) (Left D) @?= Cost 5,
          testCase "X" $ col (Left B) (Right X) @?= Cost 11,
          testCase "Y" $ col (Left B) (Right Y) @?= Cost 14,
          testCase "Z" $ col (Left B) (Right Z) @?= Cost 14
        ],
      testGroup
        "C"
        [ testCase "A" $ col (Left C) (Left A) @?= Cost 5,
          testCase "B" $ col (Left C) (Left B) @?= Cost 3,
          testCase "C" $ col (Left C) (Left C) @?= Cost 0,
          testCase "D" $ col (Left C) (Left D) @?= Cost 8,
          testCase "X" $ col (Left C) (Right X) @?= Cost 14,
          testCase "Y" $ col (Left C) (Right Y) @?= Cost 17,
          testCase "Z" $ col (Left C) (Right Z) @?= Cost 17
        ],
      testGroup
        "D"
        [ testCase "A" $ col (Left D) (Left A) @?= Cost 9,
          testCase "B" $ col (Left D) (Left B) @?= Cost 7,
          testCase "C" $ col (Left D) (Left C) @?= Cost 4,
          testCase "D" $ col (Left D) (Left D) @?= Cost 0,
          testCase "X" $ col (Left D) (Right X) @?= Cost 12,
          testCase "Y" $ col (Left D) (Right Y) @?= Cost 9,
          testCase "Z" $ col (Left D) (Right Z) @?= Cost 15
        ],
      testGroup
        "X"
        [ testCase "A" $ col (Right X) (Left A) @?= Infinity,
          testCase "B" $ col (Right X) (Left B) @?= Infinity,
          testCase "C" $ col (Right X) (Left C) @?= Infinity,
          testCase "D" $ col (Right X) (Left D) @?= Infinity,
          testCase "X" $ col (Right X) (Right X) @?= Cost 0,
          testCase "Y" $ col (Right X) (Right Y) @?= Cost 4,
          testCase "Z" $ col (Right X) (Right Z) @?= Cost 3
        ],
      testGroup
        "Y"
        [ testCase "A" $ col (Right Y) (Left A) @?= Infinity,
          testCase "B" $ col (Right Y) (Left B) @?= Infinity,
          testCase "C" $ col (Right Y) (Left C) @?= Infinity,
          testCase "D" $ col (Right Y) (Left D) @?= Infinity,
          testCase "X" $ col (Right Y) (Right X) @?= Cost 3,
          testCase "Y" $ col (Right Y) (Right Y) @?= Cost 0,
          testCase "Z" $ col (Right Y) (Right Z) @?= Cost 6
        ],
      testGroup
        "Z"
        [ testCase "A" $ col (Right Z) (Left A) @?= Infinity,
          testCase "B" $ col (Right Z) (Left B) @?= Infinity,
          testCase "C" $ col (Right Z) (Left C) @?= Infinity,
          testCase "D" $ col (Right Z) (Left D) @?= Infinity,
          testCase "X" $ col (Right Z) (Right X) @?= Cost 7,
          testCase "Y" $ col (Right Z) (Right Y) @?= Cost 4,
          testCase "Z" $ col (Right Z) (Right Z) @?= Cost 0
        ]
    ]
