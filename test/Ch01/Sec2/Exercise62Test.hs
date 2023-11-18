module Ch01.Sec2.Exercise62Test (tests) where

import Ch01.Preorder
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.Labeled

prop_exercise_62 :: Property
prop_exercise_62 = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant (-10) 10)
  (Labeled _ f) <-
    forAll $ Gen.element [Labeled "+" (+ n), Labeled "*" (* n), Labeled "-" ((-) n)]
  (Labeled _ lte) <-
    forAll $ Gen.element [Labeled "<=" (<=), Labeled ">=" (>=), Labeled "==" (==)]

  let xs = [-10 .. 10]
      ys = [-100 .. 100]

  let dpo = Preorder (==) xs
      po = Preorder lte ys

  x <- forAll $ Gen.int (Range.constant (-10) 10)
  y <- forAll $ Gen.int (Range.constant (-10) 10)

  -- from the exercise:
  isLte dpo x y ==> isLte po (f x) (f y)

  -- since:
  (isLte dpo x y) === (x == y)

  -- this is just saying that every preorder is reflexive:
  H.assert $ isLte po (f x) (f x)

tests :: TestTree
tests = testProperty "Ch01.Sec2.Exercise62Test" prop_exercise_62
