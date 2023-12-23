module Ch2.Sec2.BooleanMonoids
  ( BooleanAnd (..),
    BooleanOr (..),
  )
where

import Lib.Preorder

-- and
newtype BooleanAnd = BooleanAnd Bool deriving (Show, Eq, Ord)

instance Preorder BooleanAnd where
  lte = (<=)

instance Monoid BooleanAnd where
  mempty = BooleanAnd True

instance Semigroup BooleanAnd where
  (BooleanAnd x) <> (BooleanAnd y) = BooleanAnd (x && y)

-- or
newtype BooleanOr = BooleanOr Bool deriving (Show, Eq, Ord)

instance Preorder BooleanOr where
  lte = (<=)

instance Monoid BooleanOr where
  mempty = BooleanOr False

instance Semigroup BooleanOr where
  (BooleanOr x) <> (BooleanOr y) = BooleanOr (x || y)