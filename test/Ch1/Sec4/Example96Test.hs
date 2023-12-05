module Ch1.Sec4.Example96Test (tests) where

import Ch1.Sec4.PartitionAdjunctProperties
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog

prop_example96Left :: Property
prop_example96Left = property $ do
  -- set up
  let sss = [[S 1, S 3], [S 2, S 4]]
      g (S 1) = (T 12)
      g (S 2) = (T 12)
      g (S s) = (T s)

  -- exercise
  checkLeftAdjunct sss g

prop_example96Right :: Property
prop_example96Right = property $ do
  -- set up
  let ss = fmap S [1 .. 4]
      g (S 1) = (T 12)
      g (S 2) = (T 12)
      g (S s) = (T s)
      tss = [fmap T [3, 4, 12]]

  -- exercise
  checkRightAdjunct ss tss g

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Example96Test"
    [ testProperty "left" prop_example96Left,
      testProperty "right" prop_example96Right
    ]
