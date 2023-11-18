module Ch01.Sec2.Example57Test (tests) where

import Ch01.Set
import Data.List (nub)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_example57 :: Property
prop_example57 = property $ do
  -- set up
  xss <- forAll $ powerSet <$> nub <$> Gen.list (Range.constant 1 20) Gen.alpha

  xs <- forAll $ Gen.element xss
  ys <- forAll $ Gen.element xss

  -- exercise and verify
  (xs `isSubsetOf` ys) ==> (length xs <= length ys)

tests :: TestTree
tests = testProperty "Ch01.Sec2.Example57Test" prop_example57
