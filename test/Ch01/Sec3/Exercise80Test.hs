module Ch01.Sec3.Exercise80Test (tests) where

import Ch01.Preorder (Preorder (..))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_exercise80 :: Property
prop_exercise80 = property $ do
  x <- forAll $ Gen.alpha
  assertMeet (Preorder (<=) [x]) [x] x

tests :: TestTree
tests = testProperty "Ch01.Sec3.Exercise80Test" prop_exercise80