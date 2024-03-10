module Ch3.Sec4.AdjunctionTest (tests) where

import Ch3.Sec4.AdjunctionExample
import Data.Functor.Adjunction
import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_unit :: Property
prop_unit = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise and verify
  unit x === (D . C $ x)

prop_counit :: Property
prop_counit = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise and verify
  counit (C . D $ x) === x

aToInt :: Int -> C Int -> Int
aToInt n (C a) = a + n

intToD :: Int -> Int -> D Int
intToD n a = D $ a + n

prop_leftAdjunct :: Property
prop_leftAdjunct = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  n <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  let f = aToInt n :: C Int -> Int
  let g = leftAdjunct f :: Int -> D Int

  -- exercise and verify
  g x === D (x + n)
  (g . f) (C x) === D (x + 2 * n)

prop_rightAdjunct :: Property
prop_rightAdjunct = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  n <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  let g = intToD n :: Int -> D Int
  let f = rightAdjunct g :: C Int -> Int

  -- exercise and verify
  f (C x) === x + n
  (g . f) (C x) === D (x + 2 * n)

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec4.AdjunctionTest"
    [ testGroup
        "unit"
        [ testProperty "unit" prop_unit
        ],
      testGroup
        "counit"
        [ testProperty "counit" prop_counit
        ],
      testProperty "left adjunct" prop_leftAdjunct,
      testProperty "right adjunct" prop_rightAdjunct
    ]
