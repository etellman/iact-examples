module Ch1.Sec3.Example79Test (tests) where

import Data.PartialOrd
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

data Ex79 = A | B | C | D deriving (Show, Eq, Ord)

instance PartialOrd Ex79 where
  C <= A = True
  C <= B = True
  C <= D = True
  D <= A = True
  D <= B = True
  D <= C = True
  x <= y = x Prelude.== y

prop_example79 :: Property
prop_example79 = property $ do
  let xs = [A, B, C, C]
      ys = [A, B]

  p <- forAll $ Gen.element [C, D]

  assertMeet xs ys p

tests :: TestTree
tests = testProperty "Ch1.Sec3.Example79Test" prop_example79
