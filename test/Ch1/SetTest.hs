module Ch1.SetTest (tests) where

import Ch1.Set
import Data.List (nub, partition, sort)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
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

-- | a non-empty set of characters
genCharSet :: Gen [Char]
genCharSet = toList <$> Gen.set (Range.constant 1 10) Gen.alpha

prop_closureBy :: Property
prop_closureBy = property $ do
  -- set up
  xss <- forAll $ toList <$> Gen.set (Range.constant 0 30) genCharSet
  cover 50 "overlaps" $ not . null $ overlaps xss
  let elements = sort . nub . concat

  -- exercise
  let yss = closureBy (==) xss

  -- verify
  elements yss === elements xss
  overlaps yss === []

prop_sameElementsBy :: Property
prop_sameElementsBy = property $ do
  -- set up
  characters <- forAll $ Gen.set (Range.constant 1 5) Gen.alpha
  xs <- forAll $ toList <$> Gen.subset characters
  ys <- forAll $ toList <$> Gen.subset characters

  cover 2 "same" $ sort xs == sort ys
  sameElementsBy (==) xs ys === (sort xs == sort ys)

tests :: TestTree
tests =
  testGroup
    "Ch1.SetTest"
    [ testProperty "power set" prop_powerSet,
      testProperty "Cartesian product" prop_cartesianProduct,
      testProperty "closure" prop_closureBy,
      testProperty "disjoint union" prop_disjointUnion,
      testProperty "same elements" prop_sameElementsBy,
      testCase "subset" $ do
        assertBool "subset" $ "a" `isSubsetOf` "ab"
        assertBool "subset" $ "" `isSubsetOf` "ab"
        assertBool "subset" $ "ab" `isSubsetOf` "acb"
    ]
