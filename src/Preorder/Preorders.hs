module Preorder.Preorders (BoolPO (..), CharPO (..), IntPO (..)) where

import Data.PartialOrd

newtype IntPO = IntPO Int deriving (Show, Eq, Ord)

instance PartialOrd IntPO where
  (<=) = (Prelude.<=)

newtype CharPO = CharPO Char deriving (Show, Eq, Ord)

instance PartialOrd CharPO where
  (<=) = (Prelude.<=)

newtype BoolPO = BoolPO Bool deriving (Show, Eq, Ord)

instance PartialOrd BoolPO where
  (<=) = (Prelude.<=)
