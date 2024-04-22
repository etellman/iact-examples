module Ch4.Sec1.Exercise5Test (tests) where

import Ch4.Sec1.Definition4
import Data.PartialOrd as PO
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (BooleanAnd (..))
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

genX :: Gen X
genX = X <$> Gen.int (Range.constant 1 100)

genY :: Gen Y
genY = Y <$> Gen.int (Range.constant 1 100)

-- x can be obtained given y
phi ::
  -- | the maximum X needed
  X ->
  -- | the minimum Y needed to make at least this much X
  Y ->
  -- | the actual X needed
  X ->
  -- | the actual Y available
  Y ->
  BooleanAnd
phi maxX minY x y = BooleanAnd (x PO.<= maxX) <> BooleanAnd (minY PO.<= y)

prop_exercise5 :: Property
prop_exercise5 = property $ do
  -- set up
  maxX <- forAll genX
  minY <- forAll genY

  x <- forAll genX
  x' <- forAll genX

  y <- forAll genY
  y' <- forAll genY

  let phi' = phi maxX minY

  -- exercise and verify
  H.assert $ (hom x' x <> phi' x y <> hom y y') PO.<= phi' x' y'

tests :: TestTree
tests = testProperty "Ch4.Sec1.Exercise5Test" prop_exercise5
