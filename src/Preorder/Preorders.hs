module Preorder.Preorders
  ( BoolPO (..),
    CharPO (..),
    IntPO (..),
  )
where

import Preorder.Preorder

newtype IntPO = IntPO Int deriving (Show, Eq, Ord)

instance Preorder IntPO where
  (<=) = (Prelude.<=)

newtype CharPO = CharPO Char deriving (Show, Eq, Ord)

instance Preorder CharPO where
  (<=) = (Prelude.<=)

newtype BoolPO = BoolPO Bool deriving (Show, Eq, Ord)

instance Preorder BoolPO where
  (<=) = (Prelude.<=)
