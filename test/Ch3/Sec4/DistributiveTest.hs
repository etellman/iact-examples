module Ch3.Sec4.DistributiveTest (tests) where

import Ch3.Sec4.AdjunctionExample
import qualified Data.Distributive as D
import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_distributeMaybe :: Property
prop_distributeMaybe = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise
  let distributed = D.distribute $ Just (B x)

  -- verify
  distributed === B (Just x)

prop_collectMaybe :: Property
prop_collectMaybe = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  n <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  let f = B . (n *)

  -- exercise
  let collected = D.collect f (Just x)

  -- verify
  collected === B (Just $ n * x)

prop_distributeList :: Property
prop_distributeList = property $ do
  -- set up
  xs <- forAll $ Gen.list (Range.constant 1 20) (Gen.int (Range.constantBounded :: Range Int))

  -- exercise
  let distributed = D.distribute $ fmap B xs

  -- verify
  distributed === B xs

prop_collectList :: Property
prop_collectList = property $ do
  -- set up
  xs <- forAll $ Gen.list (Range.constant 1 20) (Gen.int (Range.constantBounded :: Range Int))

  n <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise
  let collected = D.collect (B . (n *)) xs

  -- verify
  collected === B (fmap (n *) xs)

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec4.DistributiveTest"
    [ testGroup
        "Maybe"
        [ testProperty "distribute" prop_distributeMaybe,
          testProperty "collect" prop_collectMaybe
        ],
      testGroup
        "List"
        [ testProperty "distribute" prop_distributeList,
          testProperty "collect" prop_collectList
        ]
    ]
