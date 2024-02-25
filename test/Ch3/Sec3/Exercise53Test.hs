module Ch3.Sec3.Exercise53Test (tests) where

import Ch3.Sec3.Exercise53
import Graph.Arrow
import Hedgehog as H
import Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

prop_source :: Property
prop_source = property $ do
  -- set up
  arrowG <- forAll $ Gen.element arrowsG

  -- exercise and verify
  alphaVertex (source arrowG) === source (alphaArrow arrowG)

prop_target :: Property
prop_target = property $ do
  -- set up
  arrowG <- forAll $ Gen.element arrowsG

  -- exercise and verify
  alphaVertex (target arrowG) === target (alphaArrow arrowG)

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec3.Exercise53Test"
    [ testProperty "source" prop_source,
      testProperty "target" prop_target
    ]
