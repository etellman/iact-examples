module Ch2.Sec3.Exercise41Test (tests) where

import Ch2.Sec2.YesNoMaybe
import Ch2.Sec3.Exercise41
import Ch2.Sec3.Figure18
import Lib.ApproximateDouble (ApproximateDouble)
import Graph.Graph
import Test.Tasty
import Test.Tasty.HUnit

testProbabilities :: Vertex -> [ApproximateDouble] -> [TestTree]
testProbabilities v ws =
  let test (v2, w) =
        testCase (show v2) $
          maxPath probArrowsFrom v v2 @?= Just w
   in fmap test (zip vertices $ fmap Probability ws)

testYnm :: Vertex -> [YesNoMaybe] -> [TestTree]
testYnm v ws =
  let test (v2, w) = testCase (show v2) $ maxPath ynmArrowsFrom v v2 @?= Just w
   in fmap test (zip vertices $ fmap YnmMin ws)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Exercise41Test"
    [ testGroup
        "probabilities"
        [ testGroup "A ->" $ testProbabilities A [1, 3 * 3 / 6 ** 2, 3 / 6, 3 * 3 * 5 / 6 ** 3],
          testGroup "B ->" $ testProbabilities B [1 / 3, 1, 5 * 6 / 6 ** 2, 5 / 6],
          testGroup "C ->" $ testProbabilities C [3 * 2 / 6 ** 2, 0.5, 1, 3 * 5 / 6 ** 2],
          testGroup "D ->" $ testProbabilities D [6 * 3 * 2 / 6 ** 3, (6 * 3) / (6 ** 2), 1, 1]
        ],
      testGroup
        "yes/no/maybe"
        [ testGroup "A ->" $ testYnm A [Yes, Maybe, Maybe, Maybe],
          testGroup "B ->" $ testYnm B [No, Yes, Yes, Yes],
          testGroup "C ->" $ testYnm C [No, Maybe, Yes, Maybe],
          testGroup "D ->" $ testYnm D [No, Maybe, Yes, Yes]
        ]
    ]
