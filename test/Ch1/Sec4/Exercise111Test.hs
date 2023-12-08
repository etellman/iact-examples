module Ch1.Sec4.Exercise111Test (tests) where

import Ch1.Sec4.ModAdjunction
import Ch1.Set (isSubsetOf)
import Data.Set (fromList, toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genAs :: Gen [A]
genAs =
  fmap A . toList
    <$> Gen.set
      (Range.linear 1 20)
      (Gen.int $ Range.linear 1 1000)

genSubset :: Ord a => [a] -> Gen [a]
genSubset as = toList <$> (Gen.subset $ fromList as)

prop_exercise111 ::
  (([A] -> [A]) -> [A] -> PropertyT IO ()) ->
  Property
prop_exercise111 verify = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.linear 2 10)
  as <- forAll genAs
  let bs = bsFor n

  let f = anyA (fn n) bs
      g = fstar (fn n) as

  -- exercise and verify
  p <- forAll $ genSubset as
  cover 10 "round trip not identity" $ (g . f) p /= p
  verify (g . f) p

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Exercise111Test"
    [ testProperty "p <= (g . f) p" $
        prop_exercise111 (\rt p -> H.assert $ p `isSubsetOf` rt p),
      testProperty "p <= (g . f) p" $
        prop_exercise111 (\rt p -> rt p === (rt . rt) p)
    ]
