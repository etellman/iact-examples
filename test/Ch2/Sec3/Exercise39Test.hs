module Ch2.Sec3.Exercise39Test (tests) where

import Ch1.Graph
import Data.Monoid (Sum (..))
import Test.Tasty
import Test.Tasty.HUnit

data Vertex = A | B | C | D deriving (Eq, Show)

data Arrow = Arrow
  { from :: Vertex,
    to :: Vertex,
    weight :: Int
  }

instance Graph Vertex Arrow where
  vertices = [A, B, C, D]

  arrowsFrom A = [Arrow A C 3]
  arrowsFrom B = [Arrow B A 2, Arrow B D 5]
  arrowsFrom C = [Arrow C B 3]
  arrowsFrom D = [Arrow D C 6]

  source = from
  target = to

testPathsFrom :: Vertex -> [Int] -> [TestTree]
testPathsFrom v ws =
  let test (v2, w) =
        testCase (show v2) $
          minPath (Sum . weight) v v2 @?= Just w
   in fmap test (zip vertices (fmap Sum ws))

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Exercise39Test"
    [ testGroup "A ->" $ testPathsFrom A [0, 6, 3, 11],
      testGroup "B ->" $ testPathsFrom B [2, 0, 5, 5],
      testGroup "C ->" $ testPathsFrom C [5, 3, 0, 8],
      testGroup "D ->" $ testPathsFrom D [11, 9, 6, 0]
    ]
