module Ch4.Sec3.Exercise28Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

f :: Num n => (n, n, n) -> n
f (a, b, c) = a + b + c

companion :: (Ord n, Num n) => (n, n, n) -> n -> Bool
companion p q = f p <= q

conjoint :: (Ord n, Num n) => n -> (n, n, n) -> Bool
conjoint q p = q <= f p

prop_companion :: Property
prop_companion =
  property $ do
    -- set up
    a <- forAll $ Gen.int (Range.constant 1 300)
    b <- forAll $ Gen.int (Range.constant 1 300)
    c <- forAll $ Gen.int (Range.constant 1 300)
    d <- forAll $ Gen.int (Range.constant 1 900)

    cover 30 "<=" $ a + b + c <= d
    cover 30 ">" $ a + b + c > d

    -- exercise
    let v = companion (a, b, c) d

    -- verify
    v === (a + b + c <= d)

prop_conjoint :: Property
prop_conjoint =
  property $ do
    -- set up
    a <- forAll $ Gen.int (Range.constant 1 900)
    b <- forAll $ Gen.int (Range.constant 1 300)
    c <- forAll $ Gen.int (Range.constant 1 300)
    d <- forAll $ Gen.int (Range.constant 1 300)

    cover 30 "<=" $ a <= b + c + d
    cover 30 ">" $ a > b + c + d

    -- exercise
    let v = conjoint a (b, c, d)

    -- verify
    v === (a <= b + c + d)

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec3.Exercise28Test"
    [ testProperty "companion" prop_companion,
      testProperty "conjoint" prop_conjoint
    ]
