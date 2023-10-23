module Ch01.JoinTest (tests) where

import Ch01.Join
import Data.List (nub, sort)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.Hedgehog

-- verifies all the connections in the first group are present in the second group
connectionsPreserved :: [[Int]] -> [[Int]] -> Bool
connectionsPreserved xss yss =
  let elems = (nub . concat) xss
      pairs = (,) <$> elems <*> elems
      connectionMatches (x, y) = not (connected x y xss) || connected x y yss
   in all connectionMatches pairs

prop_join :: Property
prop_join =
  property $ do
    -- set up
    withDuplicates <-
      forAll $
        Gen.list
          (Range.constant 0 50)
          (Gen.list (Range.constant 0 10) (Gen.int $ Range.constant 1 20))

    let xss = fmap nub withDuplicates
        elems = (nub . concat) xss

    -- exercise
    let joined = join xss

    -- verify
    (sort . concat) joined === sort elems
    H.assert $ disjoint joined
    H.assert $ connectionsPreserved xss joined

tests :: TestTree
tests =
  testGroup
    "Ch01.JoinTest"
    [ testCase "disjoint" $ do
        assertBool "no overlap" (disjoint [[1, 2], [3, 4] :: [Int]])
        assertBool "overlap" ((not . disjoint) [[1, 2], [2, 3, 4] :: [Int]])
        assertBool "singleton" ((disjoint) [[1, 2] :: [Int]])
        assertBool "empty" (disjoint ([] :: [[Int]])),
      testProperty "join" prop_join,
      testCase
        "exercise 1.2"
        $ join2 [[11, 12], [13], [21], [22, 23]] [[11], [21], [12, 22], [13, 23]]
          @?= ([[11, 12, 13, 22, 23], [21]] :: [[Int]]),
      testCase
        "example 1.1.1"
        $ do
          let g1 = [[1, 2], [3]] :: [[Int]]
              g2 = ([[1], [2, 3]] :: [[Int]])
          assertBool "1 and 3 not connected in group 1" $ not $ connected 1 3 g1
          assertBool "1 and 3 not connected in group 2" $ not $ connected 1 3 g2
          assertBool "1 and 3 connected in join g1 g2" $ connected 1 3 (join2 g1 g2)
    ]
