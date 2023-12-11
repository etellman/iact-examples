module Ch1.Sec3.Example79Test (tests) where

import Lib.Preorder (Preorder (..))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype Ex79PO = Ex79PO Char deriving (Show, Eq, Ord)

instance Preorder Ex79PO where
  lte (Ex79PO 'c') (Ex79PO 'a') = True
  lte (Ex79PO 'c') (Ex79PO 'b') = True
  lte (Ex79PO 'c') (Ex79PO 'd') = True
  lte (Ex79PO 'd') (Ex79PO 'a') = True
  lte (Ex79PO 'd') (Ex79PO 'b') = True
  lte (Ex79PO 'd') (Ex79PO 'c') = True
  lte x y = x == y

prop_example79 :: Property
prop_example79 = property $ do
  let xs = fmap Ex79PO ['a' .. 'd']
      ys = fmap Ex79PO ['a', 'b']

  p <- forAll $ Gen.element $ fmap Ex79PO ['c', 'd']

  assertMeet xs ys p

tests :: TestTree
tests = testProperty "Ch1.Sec3.Example79Test" prop_example79
