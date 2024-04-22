module Ch1.Sec2.Exercise52Test (tests) where

import Ch1.Set (cartesianProduct)
import Ch1.UpperSet
import Data.List (sort)
import Data.PartialOrd as PO
import Test.Tasty
import Test.Tasty.HUnit

data Ex52 = A | B | C deriving (Eq, Show, Ord)

newtype ProductPO = ProductPO (Ex52, Int) deriving (Eq, Show, Ord)

instance PartialOrd ProductPO where
  (ProductPO (a, b)) <= (ProductPO (c, d)) = a Prelude.<= c && b Prelude.<= d

instance PartialOrd Ex52 where
  A <= C = True
  A <= B = True
  x <= y = x Prelude.== y

tests :: TestTree
tests = testCase "Ch1.Sec2.Exercise52Test" $ do
  let xs = [1, 2]
      ys = [A, B, C]

  let pairs = fmap ProductPO (cartesianProduct ys xs)

  (sort (upperSets pairs))
    @?= sort
      ( (fmap . fmap)
          ProductPO
          [ [],
            [(A, 1), (A, 2), (B, 1), (B, 2), (C, 1), (C, 2)],
            [(A, 2), (B, 1), (B, 2), (C, 1), (C, 2)],
            [(A, 2), (B, 2), (C, 1), (C, 2)],
            [(A, 2), (B, 2), (C, 2)],
            [(B, 1), (B, 2), (C, 1), (C, 2)],
            [(B, 2), (C, 1), (C, 2)],
            [(B, 2), (C, 2)],
            [(C, 1), (C, 2)],
            [(C, 2)]
          ]
      )
