module Ch2.Sec2.Example3
  ( RealPlus (..),
  )
where

import Data.Eq.Approximate
import Lib.Preorder
import TypeLevel.NaturalNumber

type ApproximateDouble = AbsolutelyApproximateValue (Digits Five) Double

newtype RealPlus = RealPlus ApproximateDouble deriving (Show, Eq, Ord)

instance Preorder RealPlus where
  lte = (<=)

instance Monoid RealPlus where
  mempty = RealPlus 0

instance Semigroup RealPlus where
  (RealPlus x) <> (RealPlus y) = RealPlus (x + y)
