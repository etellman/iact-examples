module Ch2.Sec2.Example3Test (tests) where

import qualified Ch2.MpoProperties as Mpo
import Ch2.Sec2.Example3
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty

genRealPlus :: Gen RealPlus
genRealPlus = RealPlus <$> Gen.realFloat (Range.exponentialFloat (-1000) 1000)

tests :: TestTree
tests = Mpo.symmetricMonoidalPreorder "Ch2.Sec2.Example3Test" genRealPlus
