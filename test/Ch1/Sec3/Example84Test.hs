module Ch1.Sec3.Example84Test (tests) where

import Data.Set (fromList, toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Preorder.Preorders (IntPO (..))
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

genIntSet :: Gen [IntPO]
genIntSet =
  do
    fmap IntPO
    <$> toList
    <$> Gen.set
      (Range.constant 1 20)
      (Gen.int $ (Range.constantBounded :: Range Int))

prop_meet :: Property
prop_meet = property $ do
  -- set up
  xs <- forAll $ genIntSet
  xs' <- forAll $ toList <$> Gen.subset (fromList xs)

  -- exercise
  let meet = if null xs' then maximum xs else minimum xs'

  -- verify
  assertMeet xs xs' meet

prop_join :: Property
prop_join = property $ do
  -- set up
  xs <- forAll $ genIntSet
  xs' <- forAll $ toList <$> Gen.subset (fromList xs)

  -- exercise
  let join = if null xs' then minimum xs else maximum xs'

  -- verify
  assertJoin xs xs' join

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec3.Example84Test"
    [ testProperty "meet" prop_meet,
      testProperty "join" prop_join
    ]
