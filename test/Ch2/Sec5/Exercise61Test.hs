module Ch2.Sec5.Exercise61Test (tests) where

import Data.Monoid (All (..))
import qualified Hedgehog.Gen as Gen
import Monoid.BooleanMonoids (PartialOrdAll (..))
import Preorder.MonoidalClosedProperties (testClosed)
import Test.Tasty

hom :: All -> All -> All
hom (All False) (All False) = All True
hom (All False) (All True) = All True
hom (All True) (All False) = All False
hom (All True) (All True) = All True

(-*) :: PartialOrdAll -> PartialOrdAll -> PartialOrdAll
(PartialOrdAll x) -* (PartialOrdAll y) = PartialOrdAll (hom x y)

infix 6 -*

testPartialOrdAll :: Bool -> TestTree
testPartialOrdAll x =
  let gen = PartialOrdAll . All <$> Gen.bool
      join xs = Just $ foldr min (PartialOrdAll $ All False) xs
   in testClosed (show x) gen (-*) join (PartialOrdAll $ All x)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec5.Exercise61Test"
    [ testPartialOrdAll False,
      testPartialOrdAll True
    ]
