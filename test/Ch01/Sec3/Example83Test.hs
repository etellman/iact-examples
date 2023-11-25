module Ch01.Sec3.Example83Test (tests) where

import Ch01.Preorder (Preorder (..))
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_booleans ::
  (Bool -> Bool -> Bool) ->
  (Preorder Bool -> [Bool] -> Bool -> PropertyT IO ()) ->
  Property
prop_booleans combine assertion = property $ do
  let xs = [True, False]
  assertion (Preorder (<=) xs) xs (foldr1 combine xs)

prop_meet :: Property
prop_meet = prop_booleans (&&) assertMeet

prop_join :: Property
prop_join = prop_booleans (||) assertJoin

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec3.Example83Test"
    [ testProperty "meet" prop_meet,
      testProperty "join" prop_join
    ]
