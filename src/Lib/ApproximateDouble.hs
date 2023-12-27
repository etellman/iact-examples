module Lib.ApproximateDouble (ApproximateDouble) where

import Data.Eq.Approximate
import TypeLevel.NaturalNumber

type ApproximateDouble = AbsolutelyApproximateValue (Digits Five) Double
