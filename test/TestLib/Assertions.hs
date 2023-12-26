module TestLib.Assertions
  ( (==>),
    assertMeet,
    assertJoin,
    assertGalois,
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import Lib.Preorder as PO

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>

-- | verifies that a values is a meet for a preorder and subset of elements
assertMeet' :: Show a => (a -> a -> Bool) -> [a] -> [a] -> a -> PropertyT IO ()
assertMeet' cmp xs xs' m = do
  footnote $ show m

  x <- forAll $ Gen.element xs

  if null xs'
    then assert $ x `cmp` m
    else do
      x' <- forAll $ Gen.element xs'
      assert $ m `cmp` x'

      all (x `cmp`) xs' ==> x `cmp` m

-- | verifies that a values is a meet for a preorder and subset of elements
assertMeet :: (Show a, Preorder a) => [a] -> [a] -> a -> PropertyT IO ()
assertMeet = assertMeet' (PO.<=)

-- | verifies that a values is a join for a preorder and subset of elements
assertJoin :: (Show a, Preorder a) => [a] -> [a] -> a -> PropertyT IO ()
assertJoin = assertMeet' (flip (PO.<=))

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
