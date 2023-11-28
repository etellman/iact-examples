module Ch1.Sec4.Exercise92Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

newtype X = X Int deriving (Show, Eq, Ord)

newtype Y = Y Int deriving (Show, Eq, Ord)

divide :: Int -> Y -> X
divide n (Y y) = X . floor $ (fromIntegral y :: Double) / fromIntegral n

prop_galois :: Property
prop_galois = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 2 10)

  let f (X x) = Y (n * x)
      g = divide n

  x <- forAll $ X <$> Gen.int (Range.constant 1 20)
  y <- forAll $ Y <$> Gen.int (Range.constant 1 20)

  -- exercise and verify
  (f x <= y) === (x <= g y)

tests :: TestTree
tests = testProperty "Ch1.Sec4.Exercise92Test" prop_galois
