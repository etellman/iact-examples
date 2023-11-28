module Ch1.Sec4.Example91Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

divide :: Int -> Int -> Int
divide n x = ceiling ((fromIntegral x / fromIntegral n) :: Double)

prop_galois :: Property
prop_galois = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.linear 2 100)
  let g = (n *)
      f = divide n

  p <- forAll $ Gen.int (Range.linear 1 1000)
  q <- forAll $ Gen.int (Range.linear 1 1000)

  -- exercise and verify
  (f p <= q) === (p <= g q)

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Example91Test"
    [ testProperty "galois" prop_galois
    ]
