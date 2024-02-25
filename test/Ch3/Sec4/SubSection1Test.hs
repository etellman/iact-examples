module Ch3.Sec4.SubSection1Test (tests) where

import Ch3.Sec4.SubSection1
import Hedgehog as H
import Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_source :: Property
prop_source = property $ do
  -- set up
  arrow <- forAll $ Gen.element (iVertex State)
  let source = (iArrow . fArrow) (ArrowGR "source" (GrArrow, GrVertex))

  -- exercise and verify
  source arrow === arrow

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec4.SubSection1Test"
    [ testGroup
        "I . F"
        [ testProperty "source" prop_source,
          testCase "target" $ do
            -- set up
            let target = (iArrow . fArrow) (ArrowGR "target" (GrArrow, GrVertex))
            target 1 @?= 4
            target 2 @?= 4
            target 3 @?= 5
            target 4 @?= 5
            target 5 @?= 5
            target 6 @?= 7
            target 7 @?= 6
        ]
    ]
