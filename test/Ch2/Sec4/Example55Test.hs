module Ch2.Sec4.Example55Test (tests) where

import Ch2.Sec4.Example55
import Graph.IntWeight
import Graph.Path
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_distance :: Property
prop_distance = property $ do
  -- set up
  v1@(XYVertex (x1, y1)) <- forAll $ Gen.element xyvertices
  v2@(XYVertex (x2, y2)) <- forAll $ Gen.element xyvertices

  let xpath = costPath toCost xarrows
      ypath = costPath toCost yarrows
      xypath = costPath toCost xyarrows

  -- exercise and verify
  xpath x1 x2 <> ypath y1 y2 === xypath v1 v2

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec4.Example55Test"
    [ testGroup
        "arrows"
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
          testCase "(C, Q)" $
            xyarrows (XYVertex (C, Q))
              @?= [XYArrow (XYVertex (C, Q)) (XYVertex (C, P)) 8]
        ],
      testProperty "distance" prop_distance
    ]
