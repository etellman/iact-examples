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
import Preorder.Preorder as PO
import Preorder.Preorders (CharPO (..))
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
  xs <- forAll $ genChars CharPO
  p <- forAll $ Gen.element xs

  -- exercise
  let ap = arrow p xs

  -- verify
  H.assert $ isUpperSet ap xs

prop_part_2_and_3 :: Property
prop_part_2_and_3 = property $ do
  -- set up
  xs <- forAll $ genChars CharPO

  -- exercise and verify
  p <- forAll $ Gen.element xs
  q <- forAll $ Gen.element xs
  let arrow' x = arrow x xs

  q PO.<= p ==> (CharSetPO $ arrow' p) PO.<= (CharSetPO $ arrow' q)
  p PO.<= q === arrow' q `isSubsetOf` arrow' p

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec2.Exercise61Test"
    [ testProperty "part 1" prop_part_1,
      testProperty "parts 2 and 3" $ prop_part_2_and_3,
      testGroup
        "part 4"
        [ testCase "pairs" $ do
            -- exercise
            let arrowPairs = fmap (\x -> (x, arrow x allEx52s)) allEx52s

            -- exercise and verify
            arrowPairs @?= [(A, [A, B, C]), (B, [B]), (C, [C])],
          --
          testCase "upper set" $
            do
              -- set up
              let fromPairs = sort . fmap (bimap Ex52Set Ex52Set)

              -- exercise
              let us = fmap Ex52Set (upperSets allEx52s)

              --  verify
              (sort . connections) us
                @?= fromPairs
                  [ ([], []),
                    ([], [A, B, C]),
                    ([], [B]),
                    ([], [B, C]),
                    ([], [C]),
                    ([A, B, C], [A, B, C]),
                    ([B], [A, B, C]),
                    ([B], [B]),
                    ([B], [B, C]),
                    ([B, C], [A, B, C]),
                    ([B, C], [B, C]),
                    ([C], [A, B, C]),
                    ([C], [B, C]),
                    ([C], [C])
                  ],
          testCase "opposite" $ do
            -- set up
            let fromPairs = sort . fmap (bimap Ex52Op Ex52Op)

            -- exercise
            let opposite = fmap Ex52Op allEx52s

            --  verify
            (sort . connections) opposite
              @?= fromPairs [(B, A), (C, A), (A, A), (B, B), (C, C)],
          --
          testCase "opposite upper set" $
            do
              -- set up
              let toOpSet = Ex52OpSet . (fmap Ex52Op)
                  fromPairs = sort . fmap (bimap toOpSet toOpSet)

              -- exercise
              let upo = upperSets $ fmap Ex52Op allEx52s

              --  verify
              (sort . connections) (fmap Ex52OpSet upo)
                @?= fromPairs
                  [ ([], []),
                    ([], [A]),
                    ([], [A, B]),
                    ([], [A, B, C]),
                    ([], [A, C]),
                    ([A], [A]),
                    ([A], [A, B]),
                    ([A], [A, B, C]),
                    ([A], [A, C]),
                    ([A, B], [A, B]),
                    ([A, B], [A, B, C]),
                    ([A, B, C], [A, B, C]),
                    ([A, C], [A, B, C]),
                    ([A, C], [A, C])
                  ]
        ]
    ]
