module Ch1.Sec2.Exercise61Test (tests) where

import Ch1.MonotoneMap
import Ch1.Sec2.Exercise61
import Ch1.Set
import Ch1.UpperSet
import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Preorder
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions

genChars :: (Char -> a) -> Gen [a]
genChars f =
  fmap f
    <$> toList
    <$> Gen.set
      (Range.constant 1 50)
      Gen.alpha

prop_part_1 :: Property
prop_part_1 = property $ do
  -- set up
  xss <- forAll $ genChars Ex61
  p <- forAll $ Gen.element xss

  -- exercise
  let ap = arrow p xss

  -- verify
  H.assert $ isUpperSet ap xss

prop_part_2_and_3 ::
  (Show a, Eq a, Preorder a, Preorder b) =>
  Gen [a] ->
  ([a] -> b) ->
  Property
prop_part_2_and_3 generator toSetPO = property $ do
  -- set up
  xs <- forAll $ generator

  -- exercise and verify
  p <- forAll $ Gen.element xs
  q <- forAll $ Gen.element xs
  let arrow' x = arrow x xs

  q `lte` p ==> (toSetPO $ arrow' p) `lte` (toSetPO $ arrow' q)
  p `lte` q === arrow' q `isSubsetOf` arrow' p

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec2.Exercise61Test"
    [ testProperty "part 1" prop_part_1,
      testProperty "parts 2 and 3" $ prop_part_2_and_3 (genChars CharPO) CharSetPO,
      testProperty "part 4 with part 2 and 3 properties" $ do
        prop_part_2_and_3 (Gen.constant $ fmap Ex61 ['a' .. 'c']) Ex61Set,
      testGroup
        "part 4"
        [ testCase "pairs" $ do
            -- set up
            let xs = fmap Ex61 ['a' .. 'c']
                fromPairs = fmap (bimap Ex61 (fmap Ex61))

            -- exercise
            let arrowPairs = fmap (\x -> (x, arrow x xs)) xs

            -- exercise and verify
            arrowPairs @?= fromPairs [('a', "abc"), ('b', "b"), ('c', "c")],
          --
          testCase "upper set" $
            do
              -- set up
              let xs = fmap Ex61 ['a' .. 'c']
                  toSet = Ex61Set . fmap Ex61
                  fromPairs = sort . fmap (bimap toSet toSet)

              -- exercise
              let us = fmap Ex61Set (upperSets xs)

              --  verify
              (sort . connections) us
                @?= fromPairs
                  [ ("", ""),
                    ("", "abc"),
                    ("", "b"),
                    ("", "bc"),
                    ("", "c"),
                    ("abc", "abc"),
                    ("b", "abc"),
                    ("b", "b"),
                    ("b", "bc"),
                    ("bc", "abc"),
                    ("bc", "bc"),
                    ("c", "abc"),
                    ("c", "bc"),
                    ("c", "c")
                  ],
          testCase "opposite" $ do
            -- set up
            let xs = fmap Ex61 ['a' .. 'c']
                fromPairs = sort . fmap (bimap (Ex61Op . Ex61) (Ex61Op . Ex61))

            -- exercise
            let opposite = fmap Ex61Op xs

            --  verify
            (sort . connections) opposite
              @?= fromPairs [('b', 'a'), ('c', 'a'), ('a', 'a'), ('b', 'b'), ('c', 'c')],
          --
          testCase "opposite upper set" $
            do
              -- set up
              let xs = fmap Ex61 ['a' .. 'c']
                  toOpSet = Ex61OpSet . (fmap (Ex61Op . Ex61))
                  fromPairs = sort . fmap (bimap toOpSet toOpSet)

              -- exercise
              let upo = upperSets $ fmap Ex61Op xs

              --  verify
              (sort . connections) (fmap Ex61OpSet upo)
                @?= fromPairs
                  [ ("", ""),
                    ("", "a"),
                    ("", "ab"),
                    ("", "abc"),
                    ("", "ac"),
                    ("a", "a"),
                    ("a", "ab"),
                    ("a", "abc"),
                    ("a", "ac"),
                    ("ab", "ab"),
                    ("ab", "abc"),
                    ("abc", "abc"),
                    ("ac", "abc"),
                    ("ac", "ac")
                  ]
        ]
    ]
