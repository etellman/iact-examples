module Ch2.Sec3.Exercise41Test (tests) where

import Ch2.Sec2.YesNoMaybe
import Ch2.Sec3.Exercise41
import Ch2.Sec3.Figure18
import Data.Matrix
import Graph.Path
import Preorder.Quantale
import Test.Tasty
import Test.Tasty.HUnit

testYnm :: Vertex -> [YesNoMaybe] -> [TestTree]
testYnm v ws =
  let test (v2, w) = testCase (show v2) $ maxPath ynmArrowsFrom v v2 @?= Just w
   in fmap test (zip vertices $ fmap YnmMin ws)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Exercise41Test"
    [ testGroup
        "yes/no/maybe"
        [ testGroup "A ->" $ testYnm A [Yes, Maybe, Maybe, Maybe],
          testGroup "B ->" $ testYnm B [No, Yes, Yes, Yes],
          testGroup "C ->" $ testYnm C [No, Maybe, Yes, Maybe],
          testGroup "D ->" $ testYnm D [No, Maybe, Yes, Yes]
        ],
      testGroup
        "Ch2.Sec3.Exercise39Test"
        [
        -- interpret the original weights sixths
        testCase "probabilities" $ do
            let weights =
                  fromLists $
                    (fmap . fmap)
                      ProbabilityWeight
                      [ [1, 0, 1 / 2, 0],
                        [1 / 3, 1, 0, 5 / 6],
                        [0, 1 / 2, 1, 0],
                        [0, 0, 1, 1]
                      ]
                expected =
                  fromLists $
                    (fmap . fmap)
                      ProbabilityWeight
                      [ [1, 1 / 4, 1 / 2, 5 / 24],
                        [1 / 3, 1, 5 / 6, 5 / 6],
                        [1 / 6, 1 / 2, 1, 5 / 12],
                        [1 / 6, 1 / 2, 1, 1]
                      ]
            distances weights @?= expected
        ]
    ]
