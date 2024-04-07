module Ch3.Sec5.Example72Test (tests) where

import Data.Bifunctor
import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_product :: Property
prop_product = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  y <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  m <- forAll $ Gen.int (Range.constantBounded :: Range Int)
  n <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  let f = (+ n)
      g = (* m)
      fg x' y' = (bimap f g) (x', y')
      px = fst
      py = snd

  let xy = fg x y

  -- exercise and verify
  px xy === f x
  py xy === g y

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec5.Example72Test"
    [ testProperty "product" prop_product
    ]
