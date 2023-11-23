module Ch01.Sec3.Definition76Test (tests) where

import Ch01.Preorder (Preorder (..))
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_definition76 :: Property
prop_definition76 = property $ do
  -- set up
  let from = 1
      to = 1000

  let xs = [from .. to]
      po = Preorder (<=) xs

  as <- forAll $ toList <$> Gen.set (Range.constant 1 20) (Gen.int $ (Range.linear from to))
  n <- forAll $ Gen.int (Range.constant 0 5)

  assertMeet po as (max from (minimum as - n))

tests :: TestTree
tests = testProperty "Ch01.Sec3.Definition76Test" prop_definition76
