module Ch1.Sec2.Exercise62Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Preorder
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.Labeled

newtype DiscretePO = DiscretePO Int deriving (Show, Eq, Ord)

instance Preorder DiscretePO where
  lte = (==)

genDiscrete :: Gen DiscretePO
genDiscrete = DiscretePO <$> Gen.int (Range.constantBounded :: Range Int)

prop_exercise_62 :: Property
prop_exercise_62 = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 2 10)
  (Labeled _ f) <-
    forAll $ Gen.element [Labeled "+" (+ n), Labeled "*" (* n), Labeled "-" ((-) n)]
  let g (DiscretePO x) = f x

  x <- forAll genDiscrete
  y <- forAll genDiscrete

  -- exercise and verify
  x `lte` y ==> g x <= g y

tests :: TestTree
tests = testProperty "Ch1.Sec2.Exercise62Test" prop_exercise_62
