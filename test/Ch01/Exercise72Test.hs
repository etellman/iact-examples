module Ch01.Exercise72Test (tests) where

import qualified Ch01.Partition as Prt
import Ch01.Preorder
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_exercise_72 :: Property
prop_exercise_72 = property $ do
  -- set up
  xs <- forAll $ toList <$> Gen.set (Range.linear 4 10) Gen.alpha
  let xss = Prt.partitions xs

  c1 <- forAll $ Gen.element xs
  c2 <- forAll $ Gen.element xs

  p1 <- forAll $ Gen.element xss
  p2 <- forAll $ Gen.element xss

  let phi = Prt.samePartition c1 c2
      connectedPreorder = Preorder Prt.isFiner xss
      boolPreorder = Preorder (<=) [False, True]

  -- exercise and verify
  isLte connectedPreorder p1 p2 ==> isLte boolPreorder (phi p1) (phi p2)

tests :: TestTree
tests = testProperty "Ch01.Exercise72Test" prop_exercise_72
