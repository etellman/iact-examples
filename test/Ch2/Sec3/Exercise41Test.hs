module Ch2.Sec3.Exercise41Test (tests) where

import Ch2.Sec3.Exercise41
import Data.Matrix
import Preorder.Quantale
import Test.Tasty
import Test.Tasty.HUnit

probabilities :: Matrix ProbabilityWeight
probabilities =
  fromLists $
    (fmap . fmap)
      ProbabilityWeight
      [ [1, 0, 1 / 2, 0],
        [1 / 3, 1, 0, 5 / 6],
        [0, 1 / 2, 1, 0],
        [0, 0, 1, 1]
      ]

toYnm :: ProbabilityWeight -> YesNoMaybe
toYnm (ProbabilityWeight 0) = No
toYnm (ProbabilityWeight 1) = Yes
toYnm (ProbabilityWeight _) = Maybe

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Exercise41Test"
    [ testCase "Probability" $ do
        let expected =
              fromLists $
                (fmap . fmap)
                  ProbabilityWeight
                  [ [1, 1 / 4, 1 / 2, 5 / 24],
                    [1 / 3, 1, 5 / 6, 5 / 6],
                    [1 / 6, 1 / 2, 1, 5 / 12],
                    [1 / 6, 1 / 2, 1, 1]
                  ]
        distances probabilities @?= expected,
      testCase "Yes/No/Maybe" $ do
        let weights = fmap toYnm probabilities
            expected =
              fromLists $
                [ [Yes, Maybe, Maybe, Maybe],
                  [Maybe, Yes, Maybe, Maybe],
                  [Maybe, Maybe, Yes, Maybe],
                  [Maybe, Maybe, Yes, Yes]
                ]
        distances weights @?= expected
    ]
