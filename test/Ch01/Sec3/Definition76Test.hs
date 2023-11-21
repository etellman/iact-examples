module Ch01.Sec3.Definition76Test (tests) where

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

  as <- forAll $ Gen.set (Range.constant 1 20) (Gen.int $ (Range.linear from to))
  n <- forAll $ Gen.int (Range.constant 0 5)

  -- select a meet
  let p = max from (minimum as - n)

  -- verify meet properties
  a <- forAll $ Gen.element $ toList as
  H.assert $ p <= a

  q <- forAll $ Gen.int (Range.constant from to)
  a <= q ==> p <= q

tests :: TestTree
tests = testProperty "Ch01.Sec3.Definition76Test" prop_definition76
