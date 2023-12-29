module Ch1.GraphTest (tests) where

import Ch1.Graph
import Control.Monad (guard)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype Vertex = Vertex Int deriving (Eq, Ord, Show)

newtype Arrow = Arrow (Vertex, Vertex)

instance Graph Vertex Arrow where
  vertices = fmap Vertex [1 .. 9]
  arrowsFrom v1@(Vertex x) = do
    i <- [0, 1]
    let y = (2 * x + i) `mod` 5
    guard $ y /= 0
    return $ Arrow (v1, Vertex y)

  source (Arrow (v, _)) = v
  target (Arrow (_, v)) = v

genVertex :: Gen Vertex
genVertex = Gen.element (vertices :: [Vertex])

prop_reflexive :: Property
prop_reflexive = property $ do
  -- set up
  v <- forAll genVertex

  -- exercise and verify
  H.assert $ isPath v v

prop_transitive :: Property
prop_transitive = property $ do
  -- set up
  v1 <- forAll genVertex
  v2 <- forAll genVertex
  v3 <- forAll genVertex

  let v1v2 = isPath v1 v2
      v1v3 = isPath v1 v3
      v2v3 = isPath v2 v3

  cover 10 "indirect route" $ v1v2 && v2v3
  cover 5 "direct and no indirect route" $ (not v1v2 || not v2v3) && v1v3
  cover 5 "no route" $ not $ v1v3

  -- exercise and verify
  v1v2 && v2v3 ==> v1v3
  not v1v3 ==> not v1v2 || not v2v3

tests :: TestTree
tests =
  testGroup
    "Ch1.GraphTest"
    [ testProperty "reflexive" prop_reflexive,
      testProperty "transitive" prop_transitive,
      testGroup
        "shortest path"
        [ testCase "1 -> 1" $ shortestPath (const 1) (Vertex 1) (Vertex 1) @?= Just 0,
          testCase "1 -> 2" $ shortestPath (const 1) (Vertex 1) (Vertex 2) @?= Just 1,
          testCase "1 -> 3" $ shortestPath (const 1) (Vertex 1) (Vertex 3) @?= Just 1,
          testCase "1 -> 4" $ shortestPath (const 1) (Vertex 1) (Vertex 4) @?= Just 2,
          testCase "3 -> 9" $ shortestPath (const 1) (Vertex 3) (Vertex 9) @?= Nothing,
          testCase "1 -> 5" $ shortestPath (const 1) (Vertex 1) (Vertex 5) @?= Nothing,
          testCase "2 -> 5" $ shortestPath (const 1) (Vertex 2) (Vertex 5) @?= Nothing,
          testCase "3 -> 7" $ shortestPath (const 1) (Vertex 3) (Vertex 7) @?= Nothing,
          testCase "1 -> 4, double cost" $
            shortestPath (const 2) (Vertex 1) (Vertex 4) @?= Just 4,
          testCase "1 -> 4, alternate cost " $
            shortestPath (\(Arrow (Vertex x, Vertex y)) -> y - x) (Vertex 1) (Vertex 4) @?= Just 3
        ]
    ]
