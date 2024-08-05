module Ch4.Sec4.Exercise39Test (tests) where

import Ch4.Sec4.Exercise39
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_f :: Property
prop_f =
  property $ do
    -- set up
    a <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let (c, d) = f a

    -- verify
    c === abs a
    d === a * 5

prop_g :: Property
prop_g =
  property $ do
    -- set up
    b <- forAll $ Gen.int (Range.constant (-100) 100)
    d <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let (e, f') = g (b, d)

    -- verify
    e === (d <= b)
    f' === d - b

prop_hTrue :: Property
prop_hTrue =
  property $ do
    -- set up
    c <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    h (c, True) === c

prop_hFalse :: Property
prop_hFalse =
  property $ do
    -- set up
    c <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    h (c, False) === 1 - c

prop_q :: Property
prop_q =
  property $ do
    -- set up
    a <- forAll $ Gen.int (Range.constant (-100) 100)
    b <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let (f', g') = q (a, b)

    -- verify
    f' === a * 5 - b
    g' === if (a * 5 <= b) then abs a else 1 - abs a

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec4.Exercise39Test"
    [ testProperty "f" prop_f,
      testProperty "g" prop_g,
      testGroup
        "h"
        [ testProperty "e == True" prop_hTrue,
          testProperty "e == False" prop_hFalse
        ],
      testProperty "q" prop_q,
      testCase "q (-2, 3)" $ q (-2, 3) @?= (-13, 2),
      testCase "q (2, 3)" $ q (2, 3) @?= (7, -1)
    ]
