module Ch2.Sec2.BooleanMonoids
  ( BooleanAnd (..),
  )
where

import Lib.Preorder

-- | +/1
newtype BooleanAnd = BooleanAnd Bool deriving (Show, Eq, Ord)

instance Preorder BooleanAnd where
  lte = (<=)

instance Monoid BooleanAnd where
  mempty = BooleanAnd True

instance Semigroup BooleanAnd where
  (BooleanAnd x) <> (BooleanAnd y) = BooleanAnd (x && y)
