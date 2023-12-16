module Ch2.Sec2.Exercise4
  ( RealTimes (..),
    ApproximateDouble,
  )
where

import Data.Eq.Approximate
import Lib.Preorder
import TypeLevel.NaturalNumber

type ApproximateDouble = AbsolutelyApproximateValue (Digits Five) Double

newtype RealTimes = RealTimes ApproximateDouble deriving (Show, Eq, Ord)

instance Preorder RealTimes where
  lte = (<=)

instance Monoid RealTimes where
  mempty = RealTimes 1

instance Semigroup RealTimes where
  (RealTimes x) <> (RealTimes y) = RealTimes (x * y)
