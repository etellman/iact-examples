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
assertMeet :: Show a => PO.Preorder a -> [a] -> a -> PropertyT IO ()
assertMeet (Preorder lte xs) xs' p = do
  x <- forAll $ Gen.element xs'
  assert $ p `lte` x

  q <- forAll $ Gen.element xs
  all (q `lte`) xs ==> p `lte` q

-- | verifies that a values is a join for a preorder and subset of elements
assertJoin :: Show a => PO.Preorder [a] -> [[a]] -> [a] -> PropertyT IO ()
assertJoin (Preorder lte xs) xs' p = do
  x <- forAll $ Gen.element xs'
  assert $ x `lte` p

  q <- forAll $ Gen.element xs
  all (`lte` q) xs ==> p `lte` q
