module Ch2.Sec5.Exercise61Test (tests) where

import qualified Hedgehog.Gen as Gen
import Monoid.BooleanMonoids (BooleanAnd (..))
import Preorder.MonoidalClosedProperties (testClosed)
import Test.Tasty

testBooleanAnd :: Bool -> TestTree
testBooleanAnd x =
  let rightAdjunct (BooleanAnd y) = BooleanAnd (x || y)
      leftAdjunct = ((BooleanAnd x) <>)
      gen = BooleanAnd <$> Gen.bool
   in testClosed (show x) gen leftAdjunct rightAdjunct

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec5.Exercise61Test"
    [ testBooleanAnd False,
      testBooleanAnd True
    ]
