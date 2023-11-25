module Ch01.Sec3.Example83Test (tests) where

import Ch01.Preorder (Preorder (..))
import Data.Set (fromList, toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_booleans ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  (Preorder Bool -> [Bool] -> Bool -> PropertyT IO ()) ->
  Property
prop_booleans combine initial verify = property $ do
  let xs = [True, False]
  xs' <- forAll $ toList <$> Gen.subset (fromList xs)

  verify (Preorder (<=) xs) xs' (foldr combine initial xs')

prop_meet :: Property
prop_meet = prop_booleans (&&) True assertMeet

prop_join :: Property
prop_join = prop_booleans (||) False assertJoin

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec3.Example83Test"
    [ testProperty "meet" prop_meet,
      testProperty "join" prop_join
    ]
