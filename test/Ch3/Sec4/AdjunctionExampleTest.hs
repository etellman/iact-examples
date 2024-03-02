module Ch3.Sec4.AdjunctionExampleTest (tests) where

import Ch3.Sec4.AdjunctionExample
import qualified Data.Distributive as D
import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_distribute :: Property
prop_distribute = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise
  let distributed = D.distribute $ Just (B x)

  -- verify
  distributed === B (Just x)

prop_collect :: Property
prop_collect = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  n <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  let f = B . (n *)

  -- exercise
  let collected = D.collect f (Just x)

  -- verify
  collected === B (Just $ n * x)

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec4.AdjunctionExampleTest"
    [ testGroup
        "Distributive"
        [ testProperty "distribute" prop_distribute,
          testProperty "collect" prop_collect
        ]
    ]
