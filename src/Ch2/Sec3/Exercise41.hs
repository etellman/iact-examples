module Ch2.Sec3.Exercise41
  ( Probability (..),
    ProbabilityArrow (..),
    probArrowsFrom,
    ynmArrowsFrom,
  )
where

import Ch2.Sec2.YesNoMaybe
import Ch2.Sec3.Figure18
import Lib.ApproximateDouble (ApproximateDouble)
import Lib.Graph

newtype Probability = Probability ApproximateDouble deriving (Eq, Ord, Show)

instance Semigroup Probability where
  Probability x <> Probability y = Probability $ x * y

instance Monoid Probability where
  mempty = Probability 1.0

data ProbabilityArrow = ProbabilityArrow
  { paFrom :: Vertex,
    paTo :: Vertex,
    paWeight :: Probability
  }

instance Arrow ProbabilityArrow Vertex Probability where
  source = paFrom
  target = paTo
  weight = paWeight

fromRatio :: Int -> Int -> Probability
fromRatio m n = Probability $ fromIntegral m / fromIntegral n

probArrowsFrom :: Vertex -> [ProbabilityArrow]
probArrowsFrom v =
  let arrows = arrowsFrom v
   in fmap (\(Fig18Arrow v1 v2 w) -> ProbabilityArrow v1 v2 (fromRatio w 6)) arrows

categorizeYnm :: Int -> YesNoMaybe
categorizeYnm d
  | d >= 5 = Yes
  | d >= 3 = Maybe
  | otherwise = No

data YnmArrow = YnmArrow
  { ynmFrom :: Vertex,
    ynmTo :: Vertex,
    ynmWeight :: YnmMin
  }

instance Arrow YnmArrow Vertex YnmMin where
  source = ynmFrom
  target = ynmTo
  weight = ynmWeight

ynmArrowsFrom :: Vertex -> [YnmArrow]
ynmArrowsFrom v =
  let arrows = arrowsFrom v
   in fmap (\(Fig18Arrow v1 v2 w) -> YnmArrow v1 v2 (YnmMin . categorizeYnm $ w)) arrows
