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
  let la = leftAdjunct :: ((Int, Int) -> Int) -> (Int -> Int -> Int)
  la f x y === f (y, x)

prop_rightAdjunct :: Property
prop_rightAdjunct = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  y <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  let f a b = a - b

  -- exercise and verify
  let ra = rightAdjunct :: (Int -> Int -> Int) -> ((Int, Int) -> Int)
  ra f (x, y) === f y x

-- from Category Theory for Programmers
-- unit is a family of morphisms in the right category (D)
-- unit_d, captures the first argument
prop_unit :: Property
prop_unit = property $ do
  -- set up
  d <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise and verify
  let ud = unit d :: Int -> (Int, Int)
  ud x === (x, d)

  -- from Category Theory for Programmers
  -- counit is a family of morphisms in the left category (C)
  -- counit_c captures applying f to c
prop_counit :: Property
prop_counit = property $ do
  -- set up
  c <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  m <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  let f = (+ m) :: Int -> Int

  -- exercise and verify
  let cu = counit :: (Int, Int -> Int) -> Int
  cu (c, f) === f c

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec4.CurryingAdjunctionTest"
    [ testProperty "left adjunct" prop_leftAdjunct,
      testProperty "right adjunct" prop_rightAdjunct,
      testProperty "unit" prop_unit,
      testProperty "counit" prop_counit
    ]
