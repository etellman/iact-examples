module Ch01.Sec1.Example48Test (tests) where

import Ch01.SetSystem
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_coarse :: Property
prop_coarse = property $ do
  -- set up
  let xs = ['a' .. 'z']
      coarse = SetSystem [xs]

  -- exercise and verify
  c <- forAll $ Gen.element xs
  labelFor coarse c === const 0 c

-- the finest partition is the identity function, if the elements are used as the partition labels
prop_fine :: Property
prop_fine = property $ do
  -- set up
  let xs = [0 .. 20] :: [Int]
      fine = SetSystem $ fmap (\c -> [c]) xs

  -- exercise and verify
  i <- forAll $ Gen.int (Range.constant 0 (length xs - 1))
  labelFor fine (xs !! i) === id i

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec1.Example48Test"
    [ testProperty "coarse" prop_coarse,
      testProperty "fine" prop_fine
    ]
