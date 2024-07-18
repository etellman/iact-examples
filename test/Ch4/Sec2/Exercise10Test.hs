module Ch4.Sec2.Exercise10Test (tests) where

import Ch4.Sec2.Example9
import Lib.VCategory
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch4.Sec2.Exercise10Test"
    [ testGroup
        "X distances"
        [ testGroup
            "A"
            [ testCase "A" $ hom A A @?= (IntCost $ Cost 0),
              testCase "B" $ hom A B @?= (IntCost $ Cost 6),
              testCase "C" $ hom A C @?= (IntCost $ Cost 3),
              testCase "D" $ hom A D @?= (IntCost $ Cost 11)
            ],
          testGroup
            "B"
            [ testCase "A" $ hom B A @?= (IntCost $ Cost 2),
              testCase "B" $ hom B B @?= (IntCost $ Cost 0),
              testCase "C" $ hom B C @?= (IntCost $ Cost 5),
              testCase "D" $ hom B D @?= (IntCost $ Cost 5)
            ],
          testGroup
            "C"
            [ testCase "A" $ hom C A @?= (IntCost $ Cost 5),
              testCase "B" $ hom C B @?= (IntCost $ Cost 3),
              testCase "C" $ hom C C @?= (IntCost $ Cost 0),
              testCase "D" $ hom C D @?= (IntCost $ Cost 8)
            ],
          testGroup
            "D"
            [ testCase "A" $ hom D A @?= (IntCost $ Cost 9),
              testCase "B" $ hom D B @?= (IntCost $ Cost 7),
              testCase "C" $ hom D C @?= (IntCost $ Cost 4),
              testCase "D" $ hom D D @?= (IntCost $ Cost 0)
            ]
        ],
      testGroup
        "X -> Y distances"
        [ testGroup
            "A"
            [ testCase "X" $ cost A X @?= Cost 17,
              testCase "Y" $ cost A Y @?= Cost 20,
              testCase "Z" $ cost A Z @?= Cost 20
            ],
          testGroup
            "B"
            [ testCase "X" $ cost B X @?= Cost 11,
              testCase "Y" $ cost B Y @?= Cost 14,
              testCase "Z" $ cost B Z @?= Cost 14
            ],
          testGroup
            "C"
            [ testCase "X" $ cost C X @?= Cost 14,
              testCase "Y" $ cost C Y @?= Cost 17,
              testCase "Z" $ cost C Z @?= Cost 17
            ],
          testGroup
            "D"
            [ testCase "X" $ cost D X @?= Cost 12,
              testCase "Y" $ cost D Y @?= Cost 9,
              testCase "Z" $ cost D Z @?= Cost 15
            ]
        ]
    ]
