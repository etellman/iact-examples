module Ch2.Sec2.RealMonoids
  ( ApproximateDouble,
    RealPlus (..),
    RealTimes (..),
    RealDiscrete (..),
  )
where

import Data.Eq.Approximate
import Lib.Preorder
import TypeLevel.NaturalNumber

type ApproximateDouble = AbsolutelyApproximateValue (Digits Five) Double

-- | +/1
newtype RealPlus = RealPlus ApproximateDouble deriving (Show, Eq, Ord)

instance Preorder RealPlus where
  (<=) = (Prelude.<=)

instance Monoid RealPlus where
  mempty = RealPlus 0

instance Semigroup RealPlus where
  (RealPlus x) <> (RealPlus y) = RealPlus (x + y)

-- | */0
newtype RealTimes = RealTimes ApproximateDouble deriving (Show, Eq, Ord)

instance Preorder RealTimes where
  (<=) = (Prelude.<=)

instance Monoid RealTimes where
  mempty = RealTimes 1

instance Semigroup RealTimes where
  (RealTimes x) <> (RealTimes y) = RealTimes (x * y)

-- | discrete preorder
newtype RealDiscrete = RealDiscrete ApproximateDouble deriving (Show, Eq, Ord)

instance Preorder RealDiscrete where
  (<=) = (==)

instance Monoid RealDiscrete where
  mempty = RealDiscrete 0

instance Semigroup RealDiscrete where
  (RealDiscrete x) <> (RealDiscrete y) = RealDiscrete (x + y)
