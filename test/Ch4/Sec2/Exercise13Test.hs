module Ch4.Sec2.Exercise13Test (tests) where

import Ch4.Sec2.Exercise13
import Data.PartialOrd as PO
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Exercise13Test"
    [ testGroup
        "Mean Spirited/Boring"
        [ testCase "$100K" $ assertBool "feasible" $ feasible (Value MeanSpirited Boring) (Cost 1),
          testCase "$500K" $ assertBool "feasible" $ feasible (Value MeanSpirited Boring) (Cost 2),
          testCase "$1M" $ assertBool "feasible" $ feasible (Value MeanSpirited Boring) (Cost 3)
        ],
      testGroup
        "Mean Spirited/Funny"
        [ testCase "not $100K" $ assertBool "feasible" $ not $ feasible (Value MeanSpirited Funny) (Cost 1),
          testCase "not $500K" $ assertBool "feasible" $ not $ feasible (Value MeanSpirited Funny) (Cost 2),
          testCase "$1M" $ assertBool "feasible" $ feasible (Value MeanSpirited Funny) (Cost 3)
        ],
      testGroup
        "Good Natured/Boring"
        [ testCase "not $100K" $ assertBool "feasible" $ not $ feasible (Value GoodNatured Boring) (Cost 1),
          testCase "$500K" $ assertBool "feasible" $ feasible (Value GoodNatured Boring) (Cost 2),
          testCase "$1M" $ assertBool "feasible" $ feasible (Value GoodNatured Boring) (Cost 3)
        ],
      testGroup
        "Good Natured/Funny"
        [ testCase "not $100K" $ assertBool "feasible" $ not $ feasible (Value GoodNatured Funny) (Cost 1),
          testCase "not $500K" $ assertBool "feasible" $ not $ feasible (Value GoodNatured Funny) (Cost 2),
          testCase "not $1M" $ assertBool "feasible" $ not $ feasible (Value GoodNatured Funny) (Cost 3)
        ],
      testGroup
        "Values"
        [ testGroup
            "Row 1"
            [ testCase "MS/B <= MS/B" $
                assertBool "<=" $
                  Value MeanSpirited Boring PO.<= Value MeanSpirited Boring,
              testCase "MS/B <= MS/F" $
                assertBool "<=" $
                  Value MeanSpirited Boring PO.<= Value MeanSpirited Funny,
              testCase "MS/B <= GN/B" $
                assertBool "<=" $
                  Value MeanSpirited Boring PO.<= Value GoodNatured Boring,
              testCase "MS/B <= GN/F" $
                assertBool "<=" $
                  Value MeanSpirited Boring PO.<= Value GoodNatured Funny
            ],
          testGroup
            "Row 2"
            [ testCase "not MS/F <= MS/B" $
                assertBool "<=" $
                  not $
                    Value MeanSpirited Funny PO.<= Value MeanSpirited Boring,
              testCase "MS/F <= MS/F" $
                assertBool "<=" $
                  Value MeanSpirited Funny PO.<= Value MeanSpirited Funny,
              testCase "not MS/F <= GN/B" $
                assertBool "<=" $
                  not $
                    Value MeanSpirited Funny PO.<= Value GoodNatured Boring,
              testCase "MS/F <= GN/F" $
                assertBool "<=" $
                  Value MeanSpirited Funny PO.<= Value GoodNatured Funny
            ],
          testGroup
            "Row 3"
            [ testCase "not GN/B <= MS/B" $
                assertBool "<=" $
                  not $
                    Value GoodNatured Boring PO.<= Value MeanSpirited Boring,
              testCase "not GN/B <= MS/F" $
                assertBool "<=" $
                  not $
                    Value GoodNatured Boring PO.<= Value MeanSpirited Funny,
              testCase "GN/B <= GN/B" $
                assertBool "<=" $
                  Value GoodNatured Boring PO.<= Value GoodNatured Boring,
              testCase "GN/B <= GN/F" $
                assertBool "<=" $
                  Value GoodNatured Boring PO.<= Value GoodNatured Funny
            ],
          testGroup
            "Row 4"
            [ testCase "not GN/F <= MS/B" $
                assertBool "<=" $
                  not $
                    Value GoodNatured Funny PO.<= Value MeanSpirited Boring,
              testCase "not GN/F <= MS/F" $
                assertBool "<=" $
                  not $
                    Value GoodNatured Funny PO.<= Value MeanSpirited Funny,
              testCase "not GN/F <= GN/B" $
                assertBool "<=" $
                  not $
                    Value GoodNatured Funny PO.<= Value GoodNatured Boring,
              testCase "GN/F <= GN/F" $
                assertBool "<=" $
                  Value GoodNatured Funny PO.<= Value GoodNatured Funny
            ]
        ]
    ]
