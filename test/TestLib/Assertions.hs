module TestLib.Assertions
  ( (==>),
    assertMeet,
    assertJoin,
    assertGalois,
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

-- | asserts that f and g form a Galois connection
assertGalois ::
  (Show p, Show q) =>
  (p -> p -> Bool) ->
  (q -> q -> Bool) ->
  Gen p ->
  Gen q ->
  (p -> q) ->
  (q -> p) ->
  PropertyT IO ()
assertGalois lteP lteQ genP genQ f g = do
  p <- forAll genP
  q <- forAll genQ

  cover 10 "f p <= q" $ f p `lteQ` q
  cover 10 "g q <= p" $ g q `lteP` p

  assert $ p `lteP` (g . f $ p)
  assert $ (f . g $ q) `lteQ` q

  f p `lteQ` q === p `lteP` g q
