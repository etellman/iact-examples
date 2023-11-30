module Ch1.Sec4.Exercise92Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

newtype P = P Int deriving (Show, Eq, Ord)

newtype Q = Q Int deriving (Show, Eq, Ord)

divide :: Int -> Q -> P
divide n (Q q) = P . floor $ (fromIntegral q :: Double) / fromIntegral n

prop_galois :: Property
prop_galois = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 2 10)

  let f (P p) = Q (n * p)
      g = divide n

  p <- forAll $ P <$> Gen.int (Range.constant 1 20)
  q <- forAll $ Q <$> Gen.int (Range.constant 1 20)

  -- exercise and verify
  (f p <= q) === (p <= g q)

tests :: TestTree
tests = testProperty "Ch1.Sec4.Exercise92Test" prop_galois
