module Ch2.Sec3.Exercise41
  ( ProbabilityWeight (..),
    YesNoMaybe (..),
  )
where

import Ch2.Sec3.Figure18
import Graph.Arrow

newtype ProbabilityWeight = ProbabilityWeight Rational deriving (Eq, Show)

instance Semigroup ProbabilityWeight where
  ProbabilityWeight x <> ProbabilityWeight y = ProbabilityWeight $ x * y

-- the best probability is the largest probability
instance Ord ProbabilityWeight where
  ProbabilityWeight x <= ProbabilityWeight y = y <= x

data YesNoMaybe = Yes | Maybe | No deriving (Eq, Ord, Show)

instance Semigroup YesNoMaybe where
  (<>) = max
