module Ch1.Sec3.Exercise80Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Preorders (IntPO (..))
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_exercise80 :: Property
prop_exercise80 = property $ do
  x <- forAll $ IntPO <$> Gen.int (Range.constantBounded :: Range Int)
  assertMeet [x] [x] x

tests :: TestTree
tests = testProperty "Ch1.Sec3.Exercise80Test" prop_exercise80
