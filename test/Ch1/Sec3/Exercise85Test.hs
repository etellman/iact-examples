{-# LANGUAGE NumericUnderscores #-}

module Ch1.Sec3.Exercise85Test (tests) where

import Data.PartialOrd
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype DividesPO = DividesPO Int deriving (Show, Eq, Ord)

instance PartialOrd DividesPO where
  (DividesPO m) <= (DividesPO n) = n `rem` m Prelude.== 0

prop_exercise85 ::
  (Int -> Int -> Int) ->
  ([DividesPO] -> [DividesPO] -> DividesPO -> PropertyT IO ()) ->
  Property
prop_exercise85 f verify = property $ do
  -- set up
  let xs = fmap DividesPO [1 .. 500_000]
      f' (DividesPO m) (DividesPO n) = DividesPO $ f m n
  xs' <-
    forAll $
      fmap DividesPO . toList
        <$> Gen.set
          (Range.linear 1 10)
          (Gen.int (Range.linear 1 15))

  -- exercise
  let actual = case xs' of
        [] -> error "empty xs'"
        (y : ys) -> foldr f' y ys

  -- verify
  verify xs xs' actual

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec3.Exercise85Test"
    [ testProperty "meet" $ prop_exercise85 gcd assertMeet,
      testProperty "join" $ prop_exercise85 lcm assertJoin
    ]
