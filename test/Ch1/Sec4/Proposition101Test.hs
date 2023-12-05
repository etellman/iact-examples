module Ch1.Sec4.Proposition101Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

newtype P = P Int deriving (Show, Eq, Ord)

newtype Q = Q Int deriving (Show, Eq, Ord)

divide :: Int -> P -> Q
divide n (P p) = Q . ceiling $ (fromIntegral p :: Double) / fromIntegral n

prop_proposition101 :: Property
prop_proposition101 = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.linear 2 10)

  let f = divide n
      g (Q q) = P (n * q)

  p <- forAll $ P <$> Gen.int (Range.constant 1 20)
  q <- forAll $ Q <$> Gen.int (Range.constant 1 20)

  -- exercise and verify
  (f p <= q) === (p <= g q)
  H.assert $ p <= (g . f) p
  H.assert $ q <= (f . g) q

tests :: TestTree
tests = testProperty "Ch1.Sec4.Proposition101Test" prop_proposition101
