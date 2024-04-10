module Ch4.Sec1.Definition1Test (tests) where

import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions ((==>))

phiN :: Int -> Int -> Int -> Bool
phiN n x y = x <= n * y

-- x can be obtained given y
-- n y's are needed to produce an x
prop_phi :: Property
prop_phi = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant 1 1000)
  x' <- forAll $ Gen.int (Range.constant 1 1000)

  y <- forAll $ Gen.int (Range.constant 1 1000)
  y' <- forAll $ Gen.int (Range.constant 1 1000)

  n <- forAll $ Gen.int (Range.constant 2 4)

  let phi = phiN n

  -- exercise and verify
  x' <= x && phi x y ==> phi x' y
  y <= y' && phi x y ==> phi x y'

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec1.Definition1Test"
    [ testProperty "Phi" prop_phi
    ]
