module Lib.MetricSpace (MetricSpace (..)) where

import Lib.ApproximateDouble (ApproximateDouble)

class MetricSpace a where
  distance :: a -> a -> ApproximateDouble
