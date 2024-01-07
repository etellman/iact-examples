module Ch2.Sec2.Exercise29Test (tests) where

import Ch2.Sec2.MonoidalMapProperties
import Monoid.NaturalMonoids
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty

genPlus :: Gen NaturalPlus
genPlus = NaturalPlus <$> Gen.int (Range.linear 0 1000)

plusToTimes :: NaturalPlus -> NaturalTimes
plusToTimes _ = NaturalTimes 1

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Exercise29Test"
    [ laxMonotoneMap genPlus plusToTimes,
      strongMonotoneMap genPlus plusToTimes,
      strictMonotoneMap genPlus plusToTimes
    ]
