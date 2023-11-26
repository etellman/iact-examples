module Ch01.Sec3.Example83Test (tests) where

import Ch01.Preorder (Preorder (..))
import Data.Monoid (All (..), Any (..))
import Data.Set (fromList, toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_booleans ::
  Monoid a =>
  (Bool -> a) ->
  (a -> Bool) ->
  (Preorder Bool -> [Bool] -> Bool -> PropertyT IO ()) ->
  Property
prop_booleans fromBool toBool verify = property $ do
  -- set up
  let xs = [True, False]
  xs' <- forAll $ toList <$> Gen.subset (fromList xs)

  -- exercise
  let meet = toBool $ mconcat (fmap fromBool xs')

  -- verify
  verify (Preorder (<=) xs) xs' meet

prop_meet :: Property
prop_meet = prop_booleans All getAll assertMeet

prop_join :: Property
prop_join = prop_booleans Any getAny assertJoin

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec3.Example83Test"
    [ testProperty "meet" prop_meet,
      testProperty "join" prop_join
    ]
