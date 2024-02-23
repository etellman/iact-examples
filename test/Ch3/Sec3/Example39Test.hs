module Ch3.Sec3.Example39Test (tests) where

import Data.Monoid (Product (..))
import Data.Semigroup.Factorial (factors)
import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_partB :: Property
prop_partB = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant 0 1000)
  let s _ = 0 :: Int

  -- exercise and verify
  (s . s) x === s x

prop_partD :: Property
prop_partD = property $ do
  -- set up
  x <- forAll $ Product <$> Gen.int (Range.constant 2 1000)
  let s n = foldr min n (factors n)

  -- exercise and verify
  (s . s) x === s x

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec3.Example39Test"
    [ testProperty "part B" prop_partB,
      testProperty "part D" prop_partD
    ]
