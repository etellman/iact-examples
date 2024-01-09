module Ch2.Sec4.Example55Test (tests) where

import Ch2.Sec4.Example55
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.Graph
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_distance :: Property
prop_distance = property $ do
  -- set up
  v1@(XYVertex (x, y)) <- forAll $ Gen.element xyvertices
  v2@(XYVertex (x', y')) <- forAll $ Gen.element xyvertices

  footnote $ show $ minPath xarrows x x'
  footnote $ show $ minPath yarrows y y'

  -- exercise and verify
  minPath xyarrows v1 v2 === minPath xarrows x x' <> minPath yarrows y y'


tests :: TestTree
tests =
  testGroup
    "Ch2.Sec4.Example55Test"
    [ testCase "(A, P)" $
        xyarrows (XYVertex (A, P))
          @?= [ XYArrow (XYVertex (A, P)) (XYVertex (B, P)) 2,
                XYArrow (XYVertex (A, P)) (XYVertex (A, Q)) 5
              ],
      testCase "(B, P)" $
        xyarrows (XYVertex (B, Q))
          @?= [ XYArrow (XYVertex (B, Q)) (XYVertex (C, Q)) 3,
                XYArrow (XYVertex (B, Q)) (XYVertex (B, P)) 8
              ],
      testProperty "distance" prop_distance
    ]
