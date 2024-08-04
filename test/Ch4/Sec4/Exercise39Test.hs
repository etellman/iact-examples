module Ch4.Sec4.Exercise39Test (tests) where

import Ch4.Sec4.Exercise39
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_fC :: Property
prop_fC =
  property $ do
    -- set up
    c <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    fC c === abs c

prop_fD :: Property
prop_fD =
  property $ do
    -- set up
    d <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    fD d === d * 5

prop_gE :: Property
prop_gE =
  property $ do
    -- set up
    d <- forAll $ Gen.int (Range.constant (-100) 100)
    b <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    gE d b === (d <= b)

prop_gF :: Property
prop_gF =
  property $ do
    -- set up
    d <- forAll $ Gen.int (Range.constant (-100) 100)
    b <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    gF d b === d - b

prop_hTrue :: Property
prop_hTrue =
  property $ do
    -- set up
    c <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    h c True === c

prop_hFalse :: Property
prop_hFalse =
  property $ do
    -- set up
    c <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    h c False === 1 - c

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec4.Exercise39Test"
    [ testProperty "fC" prop_fC,
      testProperty "fD" prop_fD,
      testProperty "gE" prop_gE,
      testProperty "gF" prop_gF,
      testGroup
        "h"
        [ testProperty "e == True" prop_hTrue,
          testProperty "e == False" prop_hFalse
        ]
    ]
