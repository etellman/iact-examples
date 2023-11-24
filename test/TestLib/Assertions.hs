module TestLib.Assertions
  ( (==>),
    assertMeet,
    assertJoin,
  )
where

import Ch01.Preorder as PO
import Hedgehog
import qualified Hedgehog.Gen as Gen
import TestLib.Generators (preorderElement)

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>

-- | verifies that a values is a meet for a preorder and subset of elements
assertMeet :: Show a => PO.Preorder a -> [a] -> a -> PropertyT IO ()
assertMeet po xs' p = do
  let lte = isLte po
  x <- forAll $ Gen.element xs'
  assert $ p `lte` x

  q <- forAll $ preorderElement po
  PO.allElements po (q `lte`) ==> p `lte` q

-- | verifies that a values is a join for a preorder and subset of elements
assertJoin :: Show a => PO.Preorder [a] -> [[a]] -> [a] -> PropertyT IO ()
assertJoin po xs' p = do
  let lte = isLte po
  x <- forAll $ Gen.element xs'
  assert $ x `lte` p

  q <- forAll $ preorderElement po
  PO.allElements po (`lte` q) ==> q `lte` p
