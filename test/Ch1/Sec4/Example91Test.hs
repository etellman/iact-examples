module Ch1.Sec4.Example91Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

newtype X = X Int deriving (Show, Eq, Ord)

newtype Y = Y Int deriving (Show, Eq, Ord)

divide :: Int -> X -> Y
divide n (X x) = Y . ceiling $ (fromIntegral x :: Double) / fromIntegral n

prop_galois :: Property
prop_galois = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.linear 2 10)

  let f = divide n
      g (Y y) = X (n * y)

  x <- forAll $ X <$> Gen.int (Range.constant 1 20)
  y <- forAll $ Y <$> Gen.int (Range.constant 1 20)

  -- exercise and verify
  (f x <= y) === (x <= g y)

tests :: TestTree
tests = testProperty "Ch1.Sec4.Example91Test" prop_galois
