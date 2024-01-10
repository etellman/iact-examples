module Monoid.NaturalMonoids
  ( NaturalPlus (..),
    NaturalTimes (..),
    NaturalDivides (..),
    NaturalPlusDivides (..),
  )
where

import Preorder.Preorder

-- | +/0
newtype NaturalPlus = NaturalPlus Int deriving (Show, Eq, Ord)

instance Preorder NaturalPlus where
  (<=) = (Prelude.<=)

instance Monoid NaturalPlus where
  mempty = NaturalPlus 0

instance Semigroup NaturalPlus where
  (NaturalPlus x) <> (NaturalPlus y) = NaturalPlus (x + y)

-- | */1
newtype NaturalTimes = NaturalTimes Int deriving (Show, Eq, Ord)

instance Preorder NaturalTimes where
  (<=) = (Prelude.<=)

instance Monoid NaturalTimes where
  mempty = NaturalTimes 1

instance Semigroup NaturalTimes where
  (NaturalTimes x) <> (NaturalTimes y) = NaturalTimes (x * y)

-- | */divides
newtype NaturalDivides = NaturalDivides Int deriving (Show, Eq, Ord)

instance Preorder NaturalDivides where
  (NaturalDivides m) <= (NaturalDivides n) = m == 0 || n `mod` m == 0

instance Monoid NaturalDivides where
  mempty = NaturalDivides 1

instance Semigroup NaturalDivides where
  (NaturalDivides x) <> (NaturalDivides y) = NaturalDivides (x * y)

-- | +/divides
newtype NaturalPlusDivides = NaturalPlusDivides Int deriving (Show, Eq, Ord)

instance Preorder NaturalPlusDivides where
  (NaturalPlusDivides m) <= (NaturalPlusDivides n) = m == 0 || n `mod` m == 0

instance Monoid NaturalPlusDivides where
  mempty = NaturalPlusDivides 0

instance Semigroup NaturalPlusDivides where
  (NaturalPlusDivides x) <> (NaturalPlusDivides y) = NaturalPlusDivides (x + y)
