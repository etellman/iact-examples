module Ch2.Sec3.Exercise39Test (tests) where

import Ch2.Sec3.Figure18
import Lib.Graph
import Test.Tasty
import Test.Tasty.HUnit

testPathsFrom :: Vertex -> [Int] -> [TestTree]
testPathsFrom v ws =
  let test (v2, w) = testCase (show v2) $ minPath arrowsFrom v v2 @?= Just w
   in fmap test (zip vertices (fmap toIntWeight ws))

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Exercise39Test"
    [ testGroup "A ->" $ testPathsFrom A [0, 6, 3, 11],
      testGroup "B ->" $ testPathsFrom B [2, 0, 5, 5],
      testGroup "C ->" $ testPathsFrom C [5, 3, 0, 8],
      testGroup "D ->" $ testPathsFrom D [11, 9, 6, 0]
    ]
