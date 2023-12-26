module Lib.Preorders
  ( BoolPO (..),
    CharPO (..),
    IntPO (..),
  )
where

import Lib.Preorder

newtype IntPO = IntPO Int deriving (Show, Eq, Ord)

instance Preorder IntPO where
  (<=) = (Prelude.<=)

newtype CharPO = CharPO Char deriving (Show, Eq, Ord)

instance Preorder CharPO where
  (<=) = (Prelude.<=)

newtype BoolPO = BoolPO Bool deriving (Show, Eq, Ord)

instance Preorder BoolPO where
  (<=) = (Prelude.<=)
