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
genIntSet = do
  fmap IntPO . toList
    <$> Gen.set
      (Range.constant 1 20)
      (Gen.int (Range.constantBounded :: Range Int))

prop_meet :: Property
prop_meet = property $ do
  -- set up
  xs <- forAll genIntSet
  as <- forAll $ toList <$> Gen.subset (fromList xs)

  -- exercise
  let meet = case (xs, as) of
                ([], _) -> error "empty xs"
                (b:bs, []) -> foldr max b bs
                (_, b:bs) -> foldr min b bs

  -- verify
  assertMeet xs as meet

prop_join :: Property
prop_join = property $ do
  -- set up
  xs <- forAll genIntSet
  as <- forAll $ toList <$> Gen.subset (fromList xs)

  -- exercise
  let join = case (xs, as) of
                ([], _) -> error "empty xs"
                (b:bs, []) -> foldr min b bs
                (_, b:bs) -> foldr max b bs

  -- verify
  assertJoin xs as join

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec3.Example84Test"
    [ testProperty "meet" prop_meet,
      testProperty "join" prop_join
    ]
