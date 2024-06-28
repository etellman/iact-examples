module Ch4.Sec1.Exercise10Test (tests) where

import Ch4.Sec1.Example7
import Data.Monoid (All (All))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Monoid.BooleanMonoids (PartialOrdAll (..))
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.Hedgehog (testProperty)

genX :: Gen X
genX = Gen.element [North, South, East, West]

genY :: Gen Y
genY = Gen.element [A, B, C, D, E]

prop_unReachable :: X -> Y -> Property
prop_unReachable x y = property $ do
  -- set up
  x' <- forAll genX
  y' <- forAll genY

  -- exercise and verify
  PartialOrdAll (All False) === reachable x x' y' y

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec1.Exercise10Test"
    [ testGroup
        "North"
        [ testGroup
            "reachable"
            [ testCase "a" $ reachable North North C A @?= PartialOrdAll (All True),
              testCase "c" $ connected North C @?= PartialOrdAll (All True),
              testCase "e" $ connected North E @?= PartialOrdAll (All True)
            ],
          testGroup
            "unreachable"
            [ testProperty "b" $ prop_unReachable North B,
              testProperty "d" $ prop_unReachable North D
            ]
        ],
      testGroup
        "South"
        [ testGroup
            "reachable"
            [ testCase "a" $ connected South A @?= PartialOrdAll (All True),
              testCase "b" $ reachable South East B B @?= PartialOrdAll (All True),
              testCase "c" $ reachable South North C C @?= PartialOrdAll (All True),
              testCase "d" $ reachable South East B D @?= PartialOrdAll (All True),
              testCase "e" $ reachable South North E E @?= PartialOrdAll (All True)
            ]
        ],
      testGroup
        "East"
        [ testGroup
            "reachable"
            [ testCase "a" $ reachable East East B A @?= PartialOrdAll (All True),
              testCase "b" $ connected East B @?= PartialOrdAll (All True),
              testCase "c" $ reachable East North C C @?= PartialOrdAll (All True),
              testCase "d" $ reachable East East B D @?= PartialOrdAll (All True),
              testCase "e" $ reachable East North E E @?= PartialOrdAll (All True)
            ]
        ],
      testGroup
        "West"
        [ testGroup
            "reachable"
            [ testCase "a" $ reachable West North C A @?= PartialOrdAll (All True),
              testCase "c" $ reachable West North C C @?= PartialOrdAll (All True),
              testCase "e" $ reachable West North E E @?= PartialOrdAll (All True)
            ],
          testGroup
            "unreachable"
            [ testProperty "b" $ prop_unReachable West B,
              testProperty "d" $ prop_unReachable West D
            ]
        ]
    ]
