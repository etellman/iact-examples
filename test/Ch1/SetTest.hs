module Ch1.SetTest (tests) where

import Ch1.Set
import Data.List (nub, partition, sort)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

prop_powerSet :: Property
prop_powerSet = property $ do
  -- set up
  xs <- forAll $ nub <$> Gen.list (Range.constant 0 8) (Gen.int $ Range.constant 0 1000)

  -- exercise
  let xss = powerSet xs

  -- verify
  length xss === 2 ^ (length xs)
  nub xss === xss
  (sort . nub . concat) xss === sort xs

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

tests :: TestTree
tests =
  testGroup
    "Ch1.SetTest"
    [ testProperty "power set" prop_powerSet,
      testProperty "Cartesian product" prop_cartesianProduct,
      testProperty
        "disjoint union"
        prop_disjointUnion,
      testCase "subset" $ do
        assertBool "subset" $ "a" `isSubsetOf` "ab"
        assertBool "subset" $ "" `isSubsetOf` "ab"
        assertBool "subset" $ "ab" `isSubsetOf` "acb"
    ]
