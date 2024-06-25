module Ch4.Sec1.Example7Test (tests) where

import Ch4.Sec1.Example7
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (PartialOrdAll (..))
import Properties.VCategoryProperties
import Test.Tasty

genX :: Gen X
genX = Gen.element [North, South, East, West]

genY :: Gen Y
genY = Gen.element [A, B, C, D, E]

genXY :: Gen XY
genXY = do
  x <- genX
  XY x <$> genY

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec1.Example7Test"
    [ vCategoryTests "X" genX (hom :: X -> X -> PartialOrdAll),
      vCategoryTests "Y" genY (hom :: Y -> Y -> PartialOrdAll),
      vCategoryTests "V-Category" genXY (hom :: XY -> XY -> PartialOrdAll)
    ]
