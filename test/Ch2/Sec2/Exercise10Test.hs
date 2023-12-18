module Ch2.Sec2.Exercise10Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions ((==>))

genInt :: Gen Int
genInt = Gen.int (Range.linear 1 4)

prop_exercise10 :: Property
prop_exercise10 = property $ do
  -- set up
  [t, u, v, w, x, y, z] <- forAll $ Gen.list (Range.singleton 7) genInt

  let p1 = t <= v + w
      p2 = w + u <= x + z
      p3 = v + x <= y
      p4 = t + u <= y + z

  -- exercise and verify
  p1 && p2 && p3 ==> p4

tests :: TestTree
tests = testProperty "Ch2.Sec2.Exercise10Test" $ prop_exercise10
