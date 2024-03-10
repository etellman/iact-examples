module Ch3.Sec4.DistributiveTest (tests) where

import Ch3.Sec4.AdjunctionExample
import qualified Data.Distributive as DI
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
  let distributed = DI.distribute $ Just (D x)

  -- verify
  distributed === D (Just x)

prop_collectMaybe :: Property
prop_collectMaybe = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  n <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  let f = D . (n *)

  -- exercise
  let collected = DI.collect f (Just x)

  -- verify
  collected === D (Just $ n * x)

prop_distributeList :: Property
prop_distributeList = property $ do
  -- set up
  xs <- forAll $ Gen.list (Range.constant 1 20) (Gen.int (Range.constantBounded :: Range Int))

  -- exercise
  let distributed = DI.distribute $ fmap D xs

  -- verify
  distributed === D xs

prop_collectList :: Property
prop_collectList = property $ do
  -- set up
  xs <- forAll $ Gen.list (Range.constant 1 20) (Gen.int (Range.constantBounded :: Range Int))
  n <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise
  let collected = DI.collect (D . (n *)) xs

  -- verify
  collected === D (fmap (n *) xs)

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
