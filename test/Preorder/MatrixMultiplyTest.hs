module Preorder.MatrixMultiplyTest (tests) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import Data.PartialOrd as PO
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Preorder.Preorder (connections)
import Preorder.Preorders (IntPO (..))
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_isomorphic :: Property
prop_isomorphic = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.linear 0 7)
  y <- forAll $ Gen.int (Range.linear 0 7)

  cover 10 "is isomorphic" $ x Prelude.== y

  (IntPO x PO.== IntPO y) === (x Prelude.== y)

prop_connections :: Property
prop_connections = property $ do
  -- set up
  xs <-
    forAll $
      fmap IntPO . toList
        <$> Gen.set
          (Range.constant 1 20)
          (Gen.int (Range.constantBounded :: Range Int))

  -- exercise
  let pairs = connections xs

  -- verify
  (x, y) <- forAll $ Gen.element pairs
  H.assert $ x Prelude.<= y

  m <- forAll $ Gen.element xs
  n <- forAll $ Gen.element xs
  m PO.<= n ==> (m, n) `Prelude.elem` pairs

  (sort . nubOrd $ fmap fst pairs) === sort xs
  (sort . nubOrd $ fmap snd pairs) === sort xs

tests :: TestTree
tests =
  testGroup
    "Preorder.PreorderTest"
    [ testProperty "isomorphic" prop_isomorphic,
      testProperty "connections" prop_connections
    ]
