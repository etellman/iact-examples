module Ch01.Exercise64Test (tests) where

import Ch01.Exercise64
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_exercise_64 :: Property
prop_exercise_64 = property $ do
  -- set up
  x <- forAll $ Gen.element xs

  let f (X n) = Y (2 * n)
      s (Y 2) = P 1
      s (Y 4) = P 2
      s (Y 6) = P 2
      s _ = undefined

  -- fs is a way of partitioning x
      fs = fstar f s

  'a' === 'a'

tests :: TestTree
tests =
  testGroup
    "Ch01.Exercise64Test"
    [ testProperty "exercise 64" $ prop_exercise_64
    ]
