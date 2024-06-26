module Ch4.Sec2.Definition4Test (tests) where

import Ch4.Sec2.Definition4
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (PartialOrdAll (..))
import Properties.VCategoryProperties
import Test.Tasty

genX :: Gen X
genX = X <$> Gen.int (Range.constant 1 1000)

genY :: Gen Y
genY = Y <$> Gen.int (Range.constant 1 1000)

genXY :: Gen XY
genXY = do
  x <- genX
  XY x <$> genY

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Definition4Test"
    [ vCategoryTests "X" genX (hom :: X -> X -> PartialOrdAll),
      vCategoryTests "Y" genY (hom :: Y -> Y -> PartialOrdAll),
      vCategoryTests "XY" genXY (hom :: XY -> XY -> PartialOrdAll)
    ]
