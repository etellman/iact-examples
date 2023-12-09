module Ch1.Sec4.Exercise117Test (tests) where

import Ch1.Set
  ( cartesianProduct,
    isSubsetOf,
    powerSet,
  )
import Data.List
  ( nub,
    partition,
    sort,
  )
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

selectRelation ::
  (((Int, Int) -> Bool) -> [(Int, Int)] -> Bool) ->
  ((Int, Int) -> Bool) ->
  [[(Int, Int)]] ->
  Gen [(Int, Int)]
selectRelation select f relations =
  Gen.element $ filter (\r -> select f r) relations

sameElements :: Ord a => [(a, a)] -> [(a, a)] -> Bool
sameElements xs ys = (nub . sort $ xs) == (nub . sort) ys

closureOp :: Ord a => (a, a) -> [(a, a)] -> [(a, a)]
closureOp (_, y) pairs =
  let (without, with) = partition (\(_, b) -> y <= b) pairs
   in foldr (\(a, b) xs -> (a, b) : (a, y) : xs) without with

closure :: Ord a => [(a, a)] -> [(a, a)]
closure [] = []
closure pairs =
  let merged = foldr closureOp pairs pairs
   in if sameElements merged pairs
        then nub pairs
        else closure merged

genInts :: Gen [Int]
genInts =
  toList
    <$> Gen.set
      (Range.constant 4 8)
      (Gen.int $ (Range.constantBounded :: Range Int))

prop_part1 :: Property
prop_part1 = property $ do
  -- set up
  ss <- forAll genInts
  let sxs = (cartesianProduct ss ss)
      us = filter (\(a, b) -> a <= b) sxs
      relations = take 5000 $ (filter (not . null) (powerSet sxs))

  q <- forAll $ selectRelation all (\(a, b) -> a <= b) relations
  cover 10 "interesting q" $ closure q /= q

  -- verify
  H.assert $ closure q `isSubsetOf` us

prop_part2 :: Property
prop_part2 = property $ do
  -- set up
  ss <- forAll genInts
  let sxs = (cartesianProduct ss ss)
      us = filter (\(a, b) -> a <= b) sxs
      relations = take 5000 $ (filter (not . null) (powerSet sxs))

  q' <- forAll $ selectRelation any (\(a, b) -> a > b) relations
  cover 50 "interesting q'" $ closure q' /= q'

  -- verify
  H.assert $ not $ closure q' `isSubsetOf` us

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Exercise117Test"
    [ testProperty "part 1" prop_part1,
      testProperty "part 2" prop_part2
    ]
