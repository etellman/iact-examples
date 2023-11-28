module Ch1.Sec2.Example21Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

prop_example_21 :: Property
prop_example_21 = property $ do
  -- set up
  let ss = [('a', [11, 12]), ('b', [13]), ('c', [21]), ('d', [22, 23])] :: [(Char, [Int])]
      f 11 = 'a'
      f 12 = 'a'
      f 13 = 'b'
      f 21 = 'c'
      f 22 = 'd'
      f 23 = 'd'
      f _ = undefined
  x <- forAll $ Gen.element (concat $ fmap snd ss)

  -- exercise
  let xs = lookup (f x) ss

  -- verify
  H.assert $ fmap (elem x) xs == Just True

tests :: TestTree
tests = testProperty "Ch1.Sec2.Example21Test" prop_example_21
