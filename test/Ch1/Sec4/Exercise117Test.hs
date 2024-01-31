module Ch1.Sec4.Exercise117Test (tests) where

import Ch1.Set (cartesianProduct, isSubsetOf)
import Data.Containers.ListUtils (nubOrd)
import Data.List (partition, sort)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

sameElements :: Ord a => [(a, a)] -> [(a, a)] -> Bool
sameElements xs ys = (nubOrd . sort $ xs) == (nubOrd . sort) ys

genPair :: [Int] -> Gen (Int, Int)
genPair xs = do
  x <- Gen.element xs
  y <- Gen.element xs

  return (x, y)

closureOp :: Ord a => (a, a) -> [(a, a)] -> [(a, a)]
closureOp (_, y) pairs =
  let (without, with) = partition (\(_, b) -> y <= b) pairs
   in foldr (\(a, b) xs -> (a, b) : (a, y) : xs) without with

closure :: Ord a => [(a, a)] -> [(a, a)]
closure [] = []
closure pairs =
  let merged = foldr closureOp pairs pairs
   in if sameElements merged pairs
        then nubOrd pairs
        else closure merged

genInts :: Gen [Int]
genInts =
  toList
    <$> Gen.set
      (Range.constant 2 20)
      (Gen.int (Range.constantBounded :: Range Int))

prop_exercise117 :: Property
prop_exercise117 = property $ do
  -- set up
  ss <- forAll genInts
  let us = filter (uncurry (<=)) (cartesianProduct ss ss)

  q <- forAll $ toList <$> Gen.set (Range.constant 1 8) (genPair ss)

  cover 50 "interesting q" $ closure q /= q
  cover 2 "subset" $ q `isSubsetOf` us
  cover 50 "not subset" $ not $ q `isSubsetOf` us

  -- verify
  q `isSubsetOf` us === closure q `isSubsetOf` us

tests :: TestTree
tests = testProperty "Ch1.Sec4.Exercise117Test" prop_exercise117
