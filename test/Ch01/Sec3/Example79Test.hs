module Ch01.Sec3.Example79Test (tests) where

import Ch01.Preorder (Preorder (..))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

lte :: Char -> Char -> Bool
lte 'c' 'a' = True
lte 'c' 'b' = True
lte 'c' 'd' = True
lte 'd' 'a' = True
lte 'd' 'b' = True
lte 'd' 'c' = True
lte x y = x == y

prop_example79 :: Property
prop_example79 = property $ do
  let xs = ['a'..'d']
      ys = ['a', 'b']
      po = Preorder lte xs

  p <- forAll $ Gen.element ['c', 'd']

  assertMeet po ys p

tests :: TestTree
tests = testProperty "Ch01.Sec3.Example79Test" prop_example79
