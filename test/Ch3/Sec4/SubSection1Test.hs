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
  let source = ArrowGR "source" (GrArrow, GrVertex)
      s = (iArrow . fArrow) source :: Int -> Int

  -- exercise and verify
  s arrow === arrow

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec4.SubSection1Test"
    [ testGroup
        "I . F"
        [ testProperty "source" prop_source,
          testCase "target" $ do
            -- set up
            let target = ArrowGR "target" (GrArrow, GrVertex)
                t = (iArrow . fArrow) target :: Int -> Int
            t 1 @?= 4
            t 2 @?= 4
            t 3 @?= 5
            t 4 @?= 5
            t 5 @?= 5
            t 6 @?= 7
            t 7 @?= 6
        ]
    ]
