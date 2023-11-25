module Ch01.Sec3.Definition76Test (tests) where

import Ch01.Meet (meet)
import Ch01.Preorder (Preorder (..))
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_nonEmpty :: Property
prop_nonEmpty = property $ do
  -- set up
  xs <-
    forAll $
      Gen.set
        (Range.constant 1 100)
        (Gen.int (Range.linearBounded :: Range Int))
  xs' <- forAll $ toList <$> Gen.subset xs

  let po = Preorder (<=) (toList xs)

  -- exercise and verify
  assertMeet po xs' (meet po xs')

prop_empty :: Property
prop_empty = property $ do
  -- set up
  xs <-
    forAll $
      Gen.set
        (Range.constant 1 100)
        (Gen.int (Range.linearBounded :: Range Int))

  -- exercise and verify
  assertMeet (Preorder (<=) (toList xs)) [] (maximum xs)

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec3.Definition76Test"
    [ testProperty "non-empty" prop_nonEmpty,
      testProperty "empty" prop_empty
    ]
