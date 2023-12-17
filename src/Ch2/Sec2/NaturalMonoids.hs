module Ch2.Sec2.NaturalMonoids
  ( NaturalPlus (..),
    NaturalTimes (..),
  )
where

import Lib.Preorder

-- | +/0
newtype NaturalPlus = NaturalPlus Int deriving (Show, Eq, Ord)

instance Preorder NaturalPlus where
  lte = (<=)

instance Monoid NaturalPlus where
  mempty = NaturalPlus 0

instance Semigroup NaturalPlus where
  (NaturalPlus x) <> (NaturalPlus y) = NaturalPlus (x + y)

-- | */1
newtype NaturalTimes = NaturalTimes Int deriving (Show, Eq, Ord)

instance Preorder NaturalTimes where
  lte = (<=)

instance Monoid NaturalTimes where
  mempty = NaturalTimes 1

instance Semigroup NaturalTimes where
  (NaturalTimes x) <> (NaturalTimes y) = NaturalTimes (x * y)
