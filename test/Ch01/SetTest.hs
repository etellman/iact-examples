module Ch01.SetTest (tests) where

import Ch01.Set
import Data.List (nub, partition)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_powerSet :: Property
prop_powerSet = property $ do
  -- set up
  xs <- forAll $ nub <$> Gen.list (Range.constant 0 8) (Gen.int $ Range.constant 0 1000)

  -- exercise
  let xss = powerSet xs

  -- verify
  length xss === 2 ^ (length xs)
  nub xss === xss
  (nub . concat) xss === xs

prop_cartesianProduct :: Property
prop_cartesianProduct = property $ do
  -- set up
  xs <- forAll $ nub <$> Gen.list (Range.constant 0 20) (Gen.int $ Range.constant 0 1000)
  ys <- forAll $ nub <$> Gen.list (Range.constant 0 20) Gen.alpha

  -- exercise
  let pairs = cartesianProduct xs ys

  -- verify
  length pairs === length xs * length ys
  H.assert $ all (\x -> elem x xs) (fmap fst pairs)
  H.assert $ all (\y -> elem y ys) (fmap snd pairs)

prop_disjointUnion :: Property
prop_disjointUnion = property $ do
  -- set up
  xs <- forAll $ nub <$> Gen.list (Range.constant 0 20) Gen.alpha
  ys <- forAll $ nub <$> Gen.list (Range.constant 0 20) Gen.alpha

  -- exercise
  let pairs = disjointUnion xs ys

  -- verify
  length pairs === length xs + length ys

  let (xs', ys') = partition (\p -> fst p == 1) pairs
  fmap snd xs' === xs
  fmap snd ys' === ys

prop_example1_21 :: Property
prop_example1_21 = property $ do
  -- set up
  let ss = [('a', [11, 12]), ('b', [13]), ('c', [21]), ('d', [22, 23])] :: [(Char, [Int])]
      f 11 = 'a'
      f 12 = 'a'
      f 13 = 'b'
      f 21 = 'c'
      f 22 = 'd'
      f 23 = 'd'
      f _ = undefined
  x <- forAll $ Gen.element ( concat $ fmap snd ss )

  -- exercise
  let xs = lookup (f x) ss

  -- verify
  H.assert $ fmap (elem x) xs == Just True

tests :: TestTree
tests =
  testGroup
    "Ch01.SetTest"
    [ testProperty "power set" prop_powerSet,
      testProperty "Cartesian product" prop_cartesianProduct,
      testProperty "disjoint union" prop_disjointUnion,
      testProperty "example 1.21" prop_example1_21
    ]
