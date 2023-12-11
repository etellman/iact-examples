module Lib.PreorderTest (tests) where

import Data.List (nub, sort)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Preorder
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_isomorphic :: Property
prop_isomorphic = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.linear 0 7)
  y <- forAll $ Gen.int (Range.linear 0 7)

  cover 10 "is isomorphic" $ x == y

  (IntPO x =~ IntPO y) === (x == y)

prop_connections :: Property
prop_connections = property $ do
  -- set up
  xs <-
    forAll $
      fmap IntPO
        <$> toList
        <$> Gen.set
          (Range.constant 1 20)
          (Gen.int $ (Range.constantBounded :: Range Int))

  -- exercise
  let pairs = connections xs

  -- verify
  (x, y) <- forAll $ Gen.element pairs
  H.assert $ x <= y

  m <- forAll $ Gen.element xs
  n <- forAll $ Gen.element xs
  m <= n ==> (m, n) `elem` pairs

  (sort . nub $ fmap fst pairs) === sort xs
  (sort . nub $ fmap snd pairs) === sort xs

tests :: TestTree
tests =
  testGroup
    "Lib.PreorderTest"
    [ testProperty "isomorphic" prop_isomorphic,
      testProperty "connections" prop_connections
    ]
