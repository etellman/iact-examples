module Ch01.Sec3.Example82Test (tests) where

import Ch01.Preorder (Preorder (..))
import Ch01.Set (isSubsetOf, powerSet)
import Data.List (intersect, union, sort)
import Data.Set (toList)
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
  xs <- forAll $ toList <$> Gen.set (Range.constant 4 10) Gen.alpha
  let xss = powerSet xs

  xs1 <- forAll $ Gen.element xss
  xs2 <- forAll $ Gen.element xss

  let xss' = [xs1, xs2]
  let x = sort $ foldr combine [] xss'

  assertion (Preorder isSubsetOf xss) xss' x

prop_meet :: Property
prop_meet = prop_powerSet intersect assertMeet

prop_join :: Property
prop_join = prop_powerSet union assertJoin

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec3.Example82Test"
    [ testProperty "meet" prop_meet,
      testProperty "join" prop_join
    ]
