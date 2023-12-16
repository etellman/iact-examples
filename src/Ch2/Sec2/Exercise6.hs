module Ch2.Sec2.Exercise6
  ( RealDiscrete (..),
    ApproximateDouble,
  )
where

import Data.Eq.Approximate
import Lib.Preorder
import TypeLevel.NaturalNumber

type ApproximateDouble = AbsolutelyApproximateValue (Digits Five) Double

newtype RealDiscrete = RealDiscrete ApproximateDouble deriving (Show, Eq, Ord)

instance Preorder RealDiscrete where
  lte = (==)

instance Monoid RealDiscrete where
  mempty = RealDiscrete 1

instance Semigroup RealDiscrete where
  (RealDiscrete x) <> (RealDiscrete y) = RealDiscrete (x * y)
