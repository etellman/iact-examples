module Ch1.Sec3.Exercise85Test (tests) where

import Ch1.Preorder (Preorder (..))
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

divides :: Int -> Int -> Bool
divides m n = n `rem` m == 0

prop_exercise85 ::
  (Int -> Int -> Int) ->
  (Preorder Int -> [Int] -> Int -> PropertyT IO ()) ->
  Property
prop_exercise85 f verify = property $ do
  -- set up
  let xs = [1 .. 500_000]
  xs' <-
    forAll $
      toList
        <$> Gen.set
          (Range.linear 1 10)
          (Gen.int $ (Range.linear 1 15))

  -- exercise and verify
  verify (Preorder divides xs) xs' (foldr1 f xs')

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec3.Exercise85Test"
    [ testProperty "meet" $ prop_exercise85 gcd assertMeet,
      testProperty "join" $ prop_exercise85 lcm assertJoin
    ]