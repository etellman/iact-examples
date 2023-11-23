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

-- | verifies that f(x) == g(x) for a reasonable number of pairs
assertMeet :: (Show a, Eq a) => Preorder a -> [a] -> a -> PropertyT IO ()
assertMeet po as p = do
  let lte = isLte po

  a <- forAll $ Gen.element as
  assert $ p `lte` a

  q <- forAll $ Gen.element $ elements po
  a `lte` q ==> p `lte` q
