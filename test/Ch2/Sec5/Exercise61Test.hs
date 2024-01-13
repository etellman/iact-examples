module Ch2.Sec5.Exercise61Test (tests) where

import qualified Hedgehog.Gen as Gen
import Monoid.BooleanMonoids (BooleanAnd (..))
import Preorder.MonoidalClosedProperties (testClosed)
import Test.Tasty

hom :: Bool -> Bool -> Bool
hom False False = True
hom False True = True
hom True False = False
hom True True = True

(-*) :: BooleanAnd -> BooleanAnd -> BooleanAnd
(BooleanAnd x) -* (BooleanAnd y) = BooleanAnd (hom x y)

testBooleanAnd :: Bool -> TestTree
testBooleanAnd x =
  let gen = BooleanAnd <$> Gen.bool
   in testClosed (show x) gen (-*) (BooleanAnd x)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec5.Exercise61Test"
    [ testBooleanAnd False,
      testBooleanAnd True
    ]
