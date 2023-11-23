module TestLib.Assertions
  ( (==>),
    assertMeet,
  )
where

import Ch01.Preorder
  ( Preorder (..),
    elements,
    isLte,
  )
import Hedgehog
import qualified Hedgehog.Gen as Gen

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>

-- | verifies that a values is a meet for a preorder and subset of elements
assertMeet :: (Show a, Eq a) => Preorder a -> [a] -> a -> PropertyT IO ()
assertMeet po xs p = do
  let lte = isLte po

  a <- forAll $ Gen.element xs
  assert $ p `lte` a

  q <- forAll $ Gen.element $ elements po
  a `lte` q ==> p `lte` q
