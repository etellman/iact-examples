module TestLib.Assertions
  ( (==>),
    assertMeet,
    assertJoin,
  )
where

import Ch01.Preorder as PO
import Hedgehog
import qualified Hedgehog.Gen as Gen

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>

-- | verifies that a values is a meet for a preorder and subset of elements
assertMeet :: (Show a, Eq a) => PO.Preorder a -> [a] -> a -> PropertyT IO ()
assertMeet po xs p = do
  let lte = PO.isLte po

  x <- forAll $ Gen.element xs
  assert $ p `lte` x

  q <- forAll $ Gen.element $ PO.elements po
  all (q `lte`) (PO.elements po) ==> p `lte` q

-- | verifies that a values is a join for a preorder and subset of elements
assertJoin :: (Show a, Eq a) => PO.Preorder [a] -> [[a]] -> [a] -> PropertyT IO ()
assertJoin po xs p = do
  let lte = PO.isLte po

  x <- forAll $ Gen.element xs
  assert $ x `lte` p

  q <- forAll $ Gen.element $ PO.elements po
  all (`lte` q) (PO.elements po) ==> p `lte` q
