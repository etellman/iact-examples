module Ch2.Sec3.Exercise41Test (tests) where

import Ch2.Sec2.YesNoMaybe
import Ch2.Sec3.Exercise41
import Ch2.Sec3.Figure18 (Arrow (..), Vertex (..))
import Lib.ApproximateDouble (ApproximateDouble)
import Lib.Graph
import Test.Tasty
import Test.Tasty.HUnit

fromRatio :: Int -> Int -> Probability
fromRatio m n = Probability $ fromIntegral m / fromIntegral n

testProbabilities :: Vertex -> [ApproximateDouble] -> [TestTree]
testProbabilities v ws =
  let test (v2, w) =
        testCase (show v2) $
          maxPath (\a -> fromRatio (weight a) 6) v v2 @?= Just w
   in fmap test (zip vertices $ fmap Probability ws)

categorizeYnm :: Int -> YesNoMaybe
categorizeYnm d
  | d >= 5 = Yes
  | d >= 3 = Maybe
  | otherwise = No

testYnm :: Vertex -> [YesNoMaybe] -> [TestTree]
testYnm v ws =
  let test (v2, w) =
        testCase (show v2) $
          maxPath (YnmMin . categorizeYnm . weight) v v2 @?= Just w
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
