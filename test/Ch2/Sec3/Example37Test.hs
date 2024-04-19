module Ch2.Sec3.Example37Test (tests) where

import Lib.MetricSpace
import Properties.MetricSpaceProperties
import Test.Tasty

newtype Ex37 = Ex37 Int deriving (Eq, Show)

instance MetricSpace Ex37 where
  distance (Ex37 x) (Ex37 y) = abs . fromIntegral $ x - y

tests :: TestTree
tests = metricSpace "Ch2.Sec3.Example37Test" $ fmap Ex37 [-100 .. 100]
