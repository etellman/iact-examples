module Ch2.Sec3.Exercise41
  ( Probability (..),
  )
where

import Lib.ApproximateDouble (ApproximateDouble)

newtype Probability = Probability ApproximateDouble deriving (Eq, Ord, Show)

instance Semigroup Probability where
  Probability x <> Probability y = Probability $ x * y

instance Monoid Probability where
  mempty = Probability 1.0
