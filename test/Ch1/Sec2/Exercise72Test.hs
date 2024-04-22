module Ch1.Sec2.Exercise72Test (tests) where

import Ch1.Partition
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.PartialOrd as PO
import Preorder.Preorders (BoolPO (..))
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype PartitionPO = PartitionPO [[Char]] deriving (Show, Eq, Ord)

instance PartialOrd PartitionPO where
  (PartitionPO xss) <= (PartitionPO yss) = isFiner xss yss

prop_exercise72 :: Property
prop_exercise72 = property $ do
  -- set up
  xs <- forAll $ toList <$> Gen.set (Range.linear 4 10) Gen.alpha
  let xss = partitions xs

  c1 <- forAll $ Gen.element xs
  c2 <- forAll $ Gen.element xs

  p1 <- forAll $ PartitionPO <$> Gen.element xss
  p2 <- forAll $ PartitionPO <$> Gen.element xss

  let phi (PartitionPO zss) = BoolPO $ samePartition zss c1 c2

  -- exercise and verify
  p1 PO.<= p2 ==> phi p1 PO.<= phi p2

tests :: TestTree
tests = testProperty "Ch1.Sec2.Exercise72Test" prop_exercise72
