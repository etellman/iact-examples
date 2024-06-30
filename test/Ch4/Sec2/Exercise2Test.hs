module Ch4.Sec2.Exercise2Test (tests) where

import Ch4.Sec2.Exercise2
import Hedgehog as H
import Hedgehog.Gen as Gen
import Data.PartialOrd as PO
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions ((==>))

genX :: Gen X
genX = Gen.element [Monoid, Category, Preorder]

genY :: Gen Y
genY = Gen.element [Book, NoBook]

genXY :: Gen XY
genXY = do
  x <- genX
  XY x <$> genY

prop_product :: Property
prop_product = property $ do
  -- set up
  x1 <- forAll genX
  x2 <- forAll genX

  y1 <- forAll genY
  y2 <- forAll genY

  x2 PO.<= x1 && y1 PO.<= y2 ==> XY x1 y1 PO.<= XY x2 y2

prop_phi :: Property
prop_phi = property $ do
  -- set up
  xy1 <- forAll genXY
  xy2 <- forAll genXY

  -- exercise and verify
  -- if xy1 is possible and xy1 is easier than xy2, then xy2 is also possible
  phi xy1 && xy1 PO.<= xy2 ==> phi xy2

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Exercise2Test"
    [ testProperty "Product" prop_product,
      testProperty "Phi" prop_phi
    ]
