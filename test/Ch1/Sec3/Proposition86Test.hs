module Ch1.Sec3.Proposition86Test (tests) where

import Ch1.Meet (join, meet)
import Data.Set (Set, fromList, toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Preorder
import Test.Tasty
import Test.Tasty.Hedgehog

genXs :: Gen (Set IntPO)
genXs =
  fromList
    <$> fmap IntPO
    <$> toList
    <$> Gen.set
      (Range.linear 1 1000)
      (Gen.int $ (Range.linearBounded :: Range Int))

prop_meet :: Property
prop_meet = property $ do
  -- set up
  xs <- forAll genXs
  ys <- forAll $ Gen.subset xs
  zs <- forAll $ Gen.subset ys
  let meetFor subXs = meet (toList xs) (toList subXs)

  -- exercise and verify
  H.assert $ meetFor ys Prelude.<= meetFor zs

prop_join :: Property
prop_join = property $ do
  -- set up
  xs <- forAll genXs
  ys <- forAll $ Gen.subset xs
  zs <- forAll $ Gen.subset ys
  let joinFor subXs = join (toList xs) (toList subXs)

  -- exercise and verify
  H.assert $ joinFor zs Prelude.<= joinFor ys

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec3.Proposition86Test"
    [ testProperty "meet" prop_meet,
      testProperty "join" prop_join
    ]
