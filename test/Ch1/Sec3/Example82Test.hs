module Ch1.Sec3.Example82Test (tests) where

import Ch1.Set (isSubsetOf, powerSet)
import Data.List (intersect, union)
import Data.Set (fromList, toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.PartialOrd as PO
import Preorder.Preorders (CharPO (..))
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype CharSetPO = CharSetPO [CharPO] deriving (Show, Eq, Ord)

instance PartialOrd CharSetPO where
  (CharSetPO x) <= (CharSetPO y) = x `isSubsetOf` y

prop_powerSet ::
  ([CharPO] -> [CharPO] -> [CharPO]) ->
  ([CharSetPO] -> [CharSetPO] -> CharSetPO -> PropertyT IO ()) ->
  Property
prop_powerSet combine assertion = property $ do
  -- set up
  xs <- forAll $ Gen.set (Range.constant 4 10) Gen.alpha
  let xss = filter (not . null) $ powerSet $ CharPO <$> toList xs

  -- make sure the list is not empty
  y <- forAll $ Gen.element xss
  xss' <- forAll $ toList <$> Gen.subset (fromList xss)

  -- exercise
  let x = foldr combine [] (y : xss')

  assertion (fmap CharSetPO xss) (fmap CharSetPO xss') (CharSetPO x)

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
