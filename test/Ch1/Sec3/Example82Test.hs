module Ch1.Sec3.Example82Test (tests) where

import Ch1.Preorder (Preorder (..))
import Ch1.Set (isSubsetOf, powerSet)
import Data.List (intersect, union)
import Data.Set (toList, fromList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_powerSet ::
  ([Char] -> [Char] -> [Char]) ->
  (Preorder [Char] -> [[Char]] -> [Char] -> PropertyT IO ()) ->
  Property
prop_powerSet combine assertion = property $ do
  xs <- forAll $ Gen.set (Range.constant 4 10) Gen.alpha
  let xss = powerSet $ toList xs

  xss' <- forAll $ toList <$> Gen.subset (fromList xss)
  let x = foldr combine [] xss'

  assertion (Preorder isSubsetOf xss) xss' x

prop_meet :: Property
prop_meet = prop_powerSet intersect assertMeet

prop_join :: Property
prop_join = prop_powerSet union assertJoin

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec3.Example82Test"
    [ testProperty "meet" prop_meet,
      testProperty "join" prop_join
    ]
