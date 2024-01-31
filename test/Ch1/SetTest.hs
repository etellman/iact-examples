module Ch1.SetTest (tests) where

import Ch1.Set
import Data.Containers.ListUtils (nubOrd)
import Data.List (partition, sort)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Slist (slist)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

safeLength :: [a] -> Int
safeLength = length . slist

prop_powerSet :: Property
prop_powerSet = property $ do
  -- set up
  xs <-
    forAll $
      nubOrd
        <$> Gen.list
          (Range.constant 0 8)
          (Gen.int $ Range.constant 0 1000)

  -- exercise
  let xss = powerSet xs

  -- verify
  safeLength xss === 2 ^ safeLength xs
  nubOrd xss === xss
  (sort . nubOrd . concat) xss === sort xs

prop_cartesianProduct :: Property
prop_cartesianProduct = property $ do
  -- set up
  xs <- forAll $ nubOrd <$> Gen.list (Range.constant 0 20) (Gen.int $ Range.constant 0 1000)
  ys <- forAll $ nubOrd <$> Gen.list (Range.constant 0 20) Gen.alpha

  -- exercise
  let pairs = cartesianProduct xs ys

  -- verify
  safeLength pairs === safeLength xs * safeLength ys
  H.assert $ all ((`elem` xs) . fst) pairs
  H.assert $ all ((`elem` ys) . snd) pairs

prop_disjointUnion :: Property
prop_disjointUnion = property $ do
  -- set up
  xs <- forAll $ nubOrd <$> Gen.list (Range.constant 0 20) Gen.alpha
  ys <- forAll $ nubOrd <$> Gen.list (Range.constant 0 20) Gen.alpha

  -- exercise
  let pairs = disjointUnion xs ys

  -- verify
  safeLength pairs === safeLength xs + safeLength ys

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
  cover 50 "overlaps" $ not . null $ overlapsBy (==) xss
  let elements = sort . nubOrd . concat

  -- exercise
  let yss = closureBy (==) xss

  -- verify
  elements yss === elements xss
  overlapsBy (==) yss === []

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
