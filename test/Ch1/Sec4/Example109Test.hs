module Ch1.Sec4.Example109Test (tests) where

import Ch1.Sec4.Example109
import Ch1.Set (isSubsetOf)
import Data.Set (fromList, toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

genAs :: Gen [A]
genAs =
  fmap A . toList
    <$> Gen.set
      (Range.linear 1 20)
      (Gen.int $ Range.linear 1 1000)

bsFor :: Int -> [B]
bsFor n = fmap B [0 .. (n - 1)]

genSubset :: Ord a => [a] -> Gen [a]
genSubset as = toList <$> (Gen.subset $ fromList as)

assertGaloisSets ::
  (Show p, Show q, Eq p, Eq q) =>
  Gen [p] ->
  Gen [q] ->
  ([p] -> [q]) ->
  ([q] -> [p]) ->
  PropertyT IO ()
assertGaloisSets = assertGalois isSubsetOf isSubsetOf

prop_fstar :: Property
prop_fstar = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.linear 3 20)
  as <- forAll genAs
  let bs = bsFor n
  let fs = fstar (fn n) as

  -- exercise and verify
  a <- forAll $ Gen.element as
  (a `elem` fs bs) === any (\b -> fn n a == b) bs

prop_anyA :: Property
prop_anyA = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.linear 3 20)
  as <- forAll genAs
  let bs = bsFor n

  let f = anyA (fn n) bs
      g = fstar (fn n) as

  -- exercise and verify
  assertGaloisSets (genSubset as) (genSubset bs) f g

prop_raAll :: Property
prop_raAll = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.linear 3 20)
  as <- forAll genAs
  let bs = bsFor n

  let f = fstar (fn n) as
      g = allAs (fn n) as bs

  -- exercise and verify
  assertGaloisSets (genSubset bs) (genSubset as) f g

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Example109Test"
    [ testProperty "f^*" prop_fstar,
      testProperty "f!" prop_anyA,
      testProperty "f_*" prop_raAll
    ]
