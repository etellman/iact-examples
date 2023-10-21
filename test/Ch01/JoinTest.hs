module Ch01.JoinTest (tests) where

import Ch01.Join
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_join :: Property
prop_join =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 0 100)
    y <- forAll $ Gen.int (Range.constant 0 100)
    let f n = n * n

    -- exercise and verify
    x >= y ==> f x >= f y

tests :: TestTree
tests =
  testGroup
    "Ch01.JoinTest"
    [ testProperty "join" prop_join,
      testCase "disjoint" $ do
        assertBool "no overlap" (disjoint [[1, 2], [3, 4] :: [Int]])
        assertBool "overlap" ((not . disjoint) [[1, 2], [2, 3, 4] :: [Int]])
        assertBool "singleton" ((disjoint) [[1, 2] :: [Int]])
        assertBool "empty" (disjoint ([] :: [[Int]]))
        assertBool "empty and non-empty" (disjoint ([[], [1]] :: [[Int]])),
      testGroup
        "join"
        [ testCase "no overlap" $ join [[1, 2], [3, 4]] @?= ([[1, 2], [3, 4]] :: [[Int]]),
          testCase "overlap" $ join [[1, 2, 3], [3, 4]] @?= ([[1, 2, 3, 4]] :: [[Int]]),
          testCase "multiple overlaps" $
            join [[1, 2], [2, 3], [3, 4]]
              @?= ([[1, 2, 3, 4]] :: [[Int]]),
          testCase
            "multiple overlaps and non-overlap"
            $ join [[1, 2], [9], [2, 3], [3, 4], [5, 6], [7, 8]]
              @?= ([[1, 2, 3, 4], [5, 6], [7, 8], [9]] :: [[Int]])
        ],
      testCase
        "exercise 1.2"
        $ join2 [[11, 12], [13], [21], [22, 23]] [[11], [21], [12, 22], [13, 23]]
          @?= ([[11, 12, 13, 22, 23], [21]] :: [[Int]]),
      testCase
        "example 1.1.1"
        $ do
          let g1 = [[1, 2], [3]] :: [[Int]]
              g2 = ([[1], [2, 3]] :: [[Int]])
          assertBool "group 1 is not connected" $ not $ connected 1 3 g1
          assertBool "group 2 is not connected" $ not $ connected 1 3 g2
          assertBool "group 1 and 2 joined is connected" $ connected 1 3 (join2 g1 g2)
    ]
