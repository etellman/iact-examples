module Ch1.Sec3.Example83Test (tests) where

import Data.Monoid (All (..), Any (..))
import Data.Set (fromList, toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.Preorders (BoolPO (..))
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_booleans ::
  Monoid a =>
  (Bool -> a) ->
  (a -> Bool) ->
  ([BoolPO] -> [BoolPO] -> BoolPO -> PropertyT IO ()) ->
  Property
prop_booleans fromBool toBool verify = property $ do
  -- set up
  let xs = [True, False]
  xs' <- forAll $ toList <$> Gen.subset (fromList xs)

  -- exercise
  let meet = toBool $ mconcat (fmap fromBool xs')

  -- verify
  verify (fmap BoolPO xs) (fmap BoolPO xs') (BoolPO meet)

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec3.Example83Test"
    [ testProperty "meet" $ prop_booleans All getAll assertMeet,
      testProperty "join" $ prop_booleans Any getAny assertJoin
    ]
