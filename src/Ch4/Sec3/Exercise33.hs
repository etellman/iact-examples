module Ch4.Sec3.Exercise33 (col, X (..), Y (..)) where

import Ch4.Sec3.Exercise15
import Monoid.Cost

col :: Either X Y -> Either X Y -> Cost Int
col (Left x) (Left x') = xDistance x x'
col (Right y) (Right y') = yDistance y y'
col (Right _) (Left _) = Infinity
col (Left x) (Right y) = phi x y
