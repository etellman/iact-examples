module Ch01.JoinTest (tests) where

import Ch01.Join
import Data.List (nub, sort)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.Hedgehog

prop_simplify :: Property
prop_simplify =
  property $ do
    -- set up
    s <-
      forAll $
        System
          <$> (fmap nub)
          <$> Gen.list
            (Range.constant 0 50)
            (Gen.list (Range.constant 0 10) (Gen.int $ Range.constant 1 20))

    -- exercise
    let js = simplify s

    -- verify
    elements js === elements s
    H.assert $ disjoint js
    H.assert $ s <= js

prop_join :: Property
prop_join =
  property $ do
    -- set up
    s1 <-
      forAll $
        System
          <$> (fmap nub)
          <$> Gen.list
            (Range.constant 0 50)
            (Gen.list (Range.constant 1 10) (Gen.int $ Range.constant 1 20))
    s2 <-
      forAll $
        System
          <$> (fmap nub)
          <$> Gen.list
            (Range.constant 0 50)
            (Gen.list (Range.constant 1 10) (Gen.int $ Range.constant 1 20))

    let combined = ((sort . nub) $ (elements s1) ++ (elements s2))

    -- exercise
    let js = join s1 s2

    -- verify
    elements js === combined
    H.assert $ disjoint js
    H.assert $ System (sets s1 ++ sets s2) <= js

tests :: TestTree
tests =
  testGroup
    "Ch01.JoinTest"
    [ testCase "disjoint" $ do
        assertBool "no overlap" $ disjoint $ System [['a', 'b'], ['c', 'd']]
        assertBool "overlap" $ not . disjoint $ System [['a', 'b'], ['b', 'c']]
        assertBool "singleton" $ disjoint $ System [['a']]
        assertBool "empty" $ disjoint $ (System [[]] :: System Char),
      testProperty "simplify" prop_simplify,
      testProperty "join" prop_join,
      testCase "exercise 1.2" $ do
        let s1 = System [[11, 12], [13], [21], [22, 23]] :: System Int
            s2 = System [[11], [21], [12, 22], [13, 23]]
        join s1 s2 @?= System [[11, 12, 13, 22, 23], [21]],
      testCase
        "example 1.1.1"
        $ do
          let s1 = System [['a', 'b'], ['c']]
              s2 = System [['a'], ['b', 'c']]
          assertBool "s1" $ not $ connected 'a' 'c' $ s1
          assertBool "s2" $ not $ connected 'a' 'c' $ s2
          assertBool "s1 v s2" $ connected 'a' 'c' $ join s1 s2,
      testCase "partition" $ do
        let xs = [1 .. 4] :: [Int]
        (length $ partitions xs) @?= 15,
      testGroup
        "distribute"
        [ testCase "non-empty" $
            distribute 'a' ["b", "c", "de"]
              @=? [["ab", "c", "de"], ["b", "ac", "de"], ["b", "c", "ade"], ["b", "c", "de", "a"]],
          testCase "empty" $ distribute 'a' [] @=? [["a"]],
          testCase "bind" $
            ([["bc", "e"], ["d"]] >>= distribute 'a')
              @?= [["abc", "e"], ["bc", "ae"], ["bc", "e", "a"], ["ad"], ["d", "a"]]
        ]
    ]
