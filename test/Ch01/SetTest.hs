module Ch01.SetTest (tests) where

import Ch01.Set
import Data.List (nub)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_powerSet :: Property
prop_powerSet = property $ do
  -- set up
  xs <- forAll $ nub <$> Gen.list (Range.constant 0 10) (Gen.int $ Range.constant 0 1000)

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

tests :: TestTree
tests =
  testGroup
    "Ch01.SetTest"
    [ testProperty "power set" prop_powerSet,
      testProperty "Cartesian product" prop_cartesianProduct
    ]
