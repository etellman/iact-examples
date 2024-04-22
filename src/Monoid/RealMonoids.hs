module Monoid.RealMonoids
  ( RealPlus (..),
    RealTimes (..),
    RealDiscrete (..),
  )
where

import Lib.ApproximateDouble
import Data.PartialOrd

-- | +/1
newtype RealPlus = RealPlus ApproximateDouble deriving (Show, Eq, Ord)

instance PartialOrd RealPlus where
  (<=) = (Prelude.<=)

instance Monoid RealPlus where
  mempty = RealPlus 0

instance Semigroup RealPlus where
  (RealPlus x) <> (RealPlus y) = RealPlus (x + y)

-- | */0
newtype RealTimes = RealTimes ApproximateDouble deriving (Show, Eq, Ord)

instance PartialOrd RealTimes where
  (<=) = (Prelude.<=)

instance Monoid RealTimes where
  mempty = RealTimes 1

instance Semigroup RealTimes where
  (RealTimes x) <> (RealTimes y) = RealTimes (x * y)

-- | discrete preorder
newtype RealDiscrete = RealDiscrete ApproximateDouble deriving (Show, Eq, Ord)

instance PartialOrd RealDiscrete where
  (<=) = (Prelude.==)

instance Monoid RealDiscrete where
  mempty = RealDiscrete 0

instance Semigroup RealDiscrete where
  (RealDiscrete x) <> (RealDiscrete y) = RealDiscrete (x + y)
