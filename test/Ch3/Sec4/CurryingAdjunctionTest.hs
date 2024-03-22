module Ch3.Sec4.CurryingAdjunctionTest (tests) where

import Data.Functor.Adjunction
import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_leftAdjunct :: Property
prop_leftAdjunct = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  y <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  let f (a, b) = a - b

  -- exercise and verify
  leftAdjunct f x y === f (y, x)

prop_rightAdjunct :: Property
prop_rightAdjunct = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  y <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  let f a b = a - b

  -- exercise and verify
  rightAdjunct f (x, y) === f y x

prop_unit :: Property
prop_unit = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  y <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise and verify
  unit y x === (x, y)

prop_counit :: Property
prop_counit = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  m <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  let f = (+ m) :: Int -> Int

  -- exercise and verify
  counit (x, f) === f x

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec4.CurryingAdjunctionTest"
    [ testProperty "left adjunct" prop_leftAdjunct,
      testProperty "right adjunct" prop_rightAdjunct,
      testProperty "unit" prop_unit,
      testProperty "counit" prop_counit
    ]
