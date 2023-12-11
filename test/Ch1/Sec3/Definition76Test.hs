module Ch1.Sec3.Definition76Test (tests) where

import Ch1.Meet (meet)
import Data.Set (fromList, toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Preorder
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions (assertMeet)

genInts :: Gen [IntPO]
genInts =
  fmap IntPO
    <$> toList
    <$> Gen.set
      (Range.constant 1 100)
      (Gen.int (Range.linearBounded :: Range Int))

prop_nonEmpty :: Property
prop_nonEmpty = property $ do
  -- set up
  xs <- forAll genInts
  xs' <- forAll $ toList <$> Gen.subset (fromList xs)

  -- exercise and verify
  assertMeet xs xs' (meet xs xs')

prop_empty :: Property
prop_empty = property $ do
  -- set up
  xs <- forAll genInts

  -- exercise and verify
  assertMeet xs [] (maximum xs)

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec3.Definition76Test"
    [ testProperty "non-empty" prop_nonEmpty,
      testProperty "empty" prop_empty
    ]
