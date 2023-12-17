module Ch2.Sec2.NaturalMonoids
  ( NaturalPlus (..),
    NaturalTimes (..),
    NaturalDivides (..),
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

-- | */divides
newtype NaturalDivides = NaturalDivides Int deriving (Show, Eq, Ord)

instance Preorder NaturalDivides where
  lte (NaturalDivides m) (NaturalDivides n) = n `mod` m == 0

instance Monoid NaturalDivides where
  mempty = NaturalDivides 1

instance Semigroup NaturalDivides where
  (NaturalDivides x) <> (NaturalDivides y) = NaturalDivides (x * y)
