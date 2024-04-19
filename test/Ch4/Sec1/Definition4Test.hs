module Ch4.Sec1.Definition4Test (tests) where

import Ch4.Sec1.Definition4
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (BooleanAnd (..))
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
    "Ch4.Sec1.Definition4Test"
    [ vCategoryTests "X" genX (hom :: X -> X -> BooleanAnd),
      vCategoryTests "Y" genY (hom :: Y -> Y -> BooleanAnd),
      vCategoryTests "XY" genXY (hom :: XY -> XY -> BooleanAnd)
    ]
