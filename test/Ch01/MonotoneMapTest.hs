module Ch01.MonotoneMapTest (tests) where

import qualified Data.Set as Set
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_example55 :: Property
prop_example55 = property $ do
  -- set up
  x <- forAll $ Gen.bool
  y <- forAll $ Gen.bool
  let f False = 17 :: Int
      f True = 24

  -- exercise and verify
  x <= y ==> f x <= f y

prop_example57 :: Property
prop_example57 = property $ do
  -- set up
  xss <-
    forAll $
      Set.powerSet
        <$> Set.fromList
        <$> Gen.list (Range.constant 1 15) Gen.alpha

  x <- forAll $ Gen.element (Set.elems xss)
  y <- forAll $ Gen.element (Set.elems xss)

  -- exercise and verify
  (x `Set.isSubsetOf` y) ==> (Set.size x <= Set.size y)

tests :: TestTree
tests =
  testGroup
    "Ch01.MonotoneMapTest"
    [ testProperty "example 1.55" prop_example55,
      testProperty "example 1.57" prop_example57
    ]
