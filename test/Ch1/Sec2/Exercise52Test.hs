module Ch1.Sec2.Exercise52Test (tests) where

import Ch1.Set (cartesianProduct)
import Ch1.UpperSet
import Data.List (sort)
import Lib.Preorder
import Test.Tasty
import Test.Tasty.HUnit

newtype ProductPO = ProductPO (Ex52PO, Int) deriving (Eq, Show, Ord)

instance Preorder ProductPO where
  lte (ProductPO (a, b)) (ProductPO (c, d)) = a <= c && b <= d

newtype Ex52PO = Ex52PO Char deriving (Eq, Ord, Show)

instance Preorder Ex52PO where
  lte (Ex52PO 'a') (Ex52PO 'c') = True
  lte (Ex52PO 'a') (Ex52PO 'b') = True
  lte x y = x == y

tests :: TestTree
tests = testCase "Ch1.Sec2.Exercise52Test" $ do
  let xs = [1, 2]
      ys = fmap Ex52PO ['a', 'b', 'c']

  let pairs = fmap ProductPO (cartesianProduct ys xs)

  (sort (upperSets pairs))
    @?= sort
      ( (fmap . fmap)
          ProductPO
          [ [],
            [(Ex52PO 'a', 1), (Ex52PO 'a', 2), (Ex52PO 'b', 1), (Ex52PO 'b', 2), (Ex52PO 'c', 1), (Ex52PO 'c', 2)],
            [(Ex52PO 'a', 2), (Ex52PO 'b', 1), (Ex52PO 'b', 2), (Ex52PO 'c', 1), (Ex52PO 'c', 2)],
            [(Ex52PO 'a', 2), (Ex52PO 'b', 2), (Ex52PO 'c', 1), (Ex52PO 'c', 2)],
            [(Ex52PO 'a', 2), (Ex52PO 'b', 2), (Ex52PO 'c', 2)],
            [(Ex52PO 'b', 1), (Ex52PO 'b', 2), (Ex52PO 'c', 1), (Ex52PO 'c', 2)],
            [(Ex52PO 'b', 2), (Ex52PO 'c', 1), (Ex52PO 'c', 2)],
            [(Ex52PO 'b', 2), (Ex52PO 'c', 2)],
            [(Ex52PO 'c', 1), (Ex52PO 'c', 2)],
            [(Ex52PO 'c', 2)]
          ]
      )
