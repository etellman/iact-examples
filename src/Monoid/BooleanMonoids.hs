module Monoid.BooleanMonoids
  ( BooleanAnd (..),
    BooleanOr (..),
  )
where

import Lib.Preorder as PO

-- and
newtype BooleanAnd = BooleanAnd Bool deriving (Show, Eq, Ord)

instance Preorder BooleanAnd where
  (<=) = (Prelude.<=)

instance Monoid BooleanAnd where
  mempty = BooleanAnd True

instance Semigroup BooleanAnd where
  (BooleanAnd x) <> (BooleanAnd y) = BooleanAnd (x && y)

-- or
newtype BooleanOr = BooleanOr Bool deriving (Show, Eq, Ord)

instance Preorder BooleanOr where
  (<=) = (Prelude.<=)

instance Monoid BooleanOr where
  mempty = BooleanOr False

instance Semigroup BooleanOr where
  (BooleanOr x) <> (BooleanOr y) = BooleanOr (x || y)
