module TestLib.Assertions
  ( (==>),
    assertMeet,
    assertJoin,
  )
where

import Ch1.Preorder as PO
import Hedgehog
import TestLib.Generators (preorderElement)

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>

-- | verifies that a values is a meet for a preorder and subset of elements
assertMeet :: Show a => PO.Preorder a -> [a] -> a -> PropertyT IO ()
assertMeet po xs' p = do
  let lte = isLte po
  assert $ all (p `lte`) xs'

  q <- forAll $ preorderElement po
  all (q `lte`) xs' ==> q `lte` p

-- | verifies that a values is a join for a preorder and subset of elements
assertJoin :: Show a => PO.Preorder a -> [a] -> a -> PropertyT IO ()
assertJoin (Preorder lte xs) xs' p = assertMeet (Preorder (flip lte) xs) xs' p
