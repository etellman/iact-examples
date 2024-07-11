module Ch2.Sec3.Exercise41
  ( ProbabilityWeight (..),
    ynmArrowsFrom,
  )
where

import Ch2.Sec2.YesNoMaybe
import Ch2.Sec3.Figure18
import Graph.Arrow

newtype ProbabilityWeight = ProbabilityWeight Rational deriving (Eq, Show)

instance Semigroup ProbabilityWeight where
  ProbabilityWeight x <> ProbabilityWeight y = ProbabilityWeight $ x * y

-- the best probability is the largest probability
instance Ord ProbabilityWeight where
  ProbabilityWeight x <= ProbabilityWeight y = y <= x

categorizeYnm :: Int -> YesNoMaybe
categorizeYnm d
  | d >= 5 = Yes
  | d >= 3 = Maybe
  | otherwise = No

data YnmArrow = YnmArrow
  { ynmFrom :: !Vertex,
    ynmTo :: !Vertex,
    ynmWeight :: !YnmMin
  }

instance Arrow YnmArrow Vertex YnmMin where
  source = ynmFrom
  target = ynmTo
  weight = ynmWeight

ynmArrowsFrom :: Vertex -> [YnmArrow]
ynmArrowsFrom v =
  let arrows = arrowsFrom v
   in fmap (\(Fig18Arrow v1 v2 w) -> YnmArrow v1 v2 (YnmMin . categorizeYnm $ w)) arrows
