module Ch2.Sec5.Proposition64Test (tests) where

import Ch1.Meet as M
import Data.PartialOrd as PO
import Data.Set (toList)
import Gen.Cost (genCostPreorder)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Monoid.Cost as C
import Test.Tasty
import Test.Tasty.Hedgehog

(-*) ::
  C.CostPreorder Int ->
  C.CostPreorder Int ->
  C.CostPreorder Int
(C.CostPreorder x) -* (C.CostPreorder y) = C.CostPreorder (x C.-* y)

infix 6 -*

partA :: Property
partA = property $ do
  -- set up
  v <- forAll genCostPreorder
  let f x = x <> v
      g x = v -* x

  p <- forAll genCostPreorder
  q <- forAll genCostPreorder

  -- exercise and verify
  f p PO.<= q === p PO.<= g q

partB :: Property
partB = property $ do
  -- set up
  v <- forAll genCostPreorder
  xsSet <- forAll $ Gen.set (Range.linear 10 50) genCostPreorder
  as <- forAll $ toList <$> Gen.subset xsSet

  let xs = toList xsSet
      j1 = M.join xs as
      j2 = M.join (fmap (v <>) xs) (fmap (v <>) as)

  -- exercise and verify
  case (j1, j2) of
    (Nothing, Nothing) -> success
    (Just j1', Just j2') -> assert $ (v <> j1') PO.== j2'
    (_, _) -> failure

partC :: Property
partC = property $ do
  -- set up
  v <- forAll genCostPreorder
  w <- forAll genCostPreorder

  -- exercise and verify
  assert $ (v <> (v -* w)) PO.<= w

partD :: Property
partD = property $ do
  -- set up
  v <- forAll genCostPreorder

  -- exercise and verify
  assert $ v PO.== (mempty <> v)

partE :: Property
partE = property $ do
  -- set up
  u <- forAll genCostPreorder
  v <- forAll genCostPreorder
  w <- forAll genCostPreorder

  -- exercise and verify
  assert $ ((u -* v) <> (v -* w)) PO.<= (u -* w)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec5.Proposition64Test"
    [ testProperty "(a)" partA,
      testProperty "(b)" partB,
      testProperty "(c)" partC,
      testProperty "(d)" partD,
      testProperty "(e)" partE
    ]
