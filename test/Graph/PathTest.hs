module Graph.PathTest (tests) where

import Control.Monad (guard)
import Graph.Arrow
import Graph.IntWeight
import Graph.Path
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype Vertex = Vertex Int deriving (Eq, Ord, Show)

newtype TestArrow = TestArrow (Vertex, Vertex)

instance Arrow TestArrow Vertex IntWeight where
  source (TestArrow (v, _)) = v
  target (TestArrow (_, v)) = v
  weight = const unitWeight

vertices :: [Vertex]
vertices = fmap Vertex [1 .. 9]

arrowsFrom :: Vertex -> [TestArrow]
arrowsFrom v1@(Vertex x) = do
  i <- [0, 1]
  let y = (2 * x + i) `mod` 5
  guard $ y /= 0
  return $ TestArrow (v1, Vertex y)

shortest :: Vertex -> Vertex -> Maybe Int
shortest v1 = fmap fromIntWeight . minPath arrowsFrom v1

prop_reflexive :: Property
prop_reflexive = property $ do
  -- set up
  v <- forAll $ Gen.element vertices

  -- exercise and verify
  H.assert $ isPath arrowsFrom v v
  Just mempty === minPath arrowsFrom v v

prop_transitive :: Property
prop_transitive = property $ do
  -- set up
  v1 <- forAll $ Gen.element vertices
  v2 <- forAll $ Gen.element vertices
  v3 <- forAll $ Gen.element vertices

  let ip = isPath arrowsFrom
      v1v2 = ip v1 v2
      v1v3 = ip v1 v3
      v2v3 = ip v2 v3

  cover 10 "indirect route" $ v1v2 && v2v3
  cover 5 "direct and no indirect route" $ (not v1v2 || not v2v3) && v1v3
  cover 5 "no route" $ not v1v3

  -- exercise and verify
  v1v2 && v2v3 ==> v1v3
  not v1v3 ==> not v1v2 || not v2v3

prop_cost :: Property
prop_cost = property $ do
  -- set up
  v1 <- forAll $ Gen.element vertices
  v2 <- forAll $ Gen.element vertices
  cover 30 "path exists" $ isPath arrowsFrom v1 v2

  -- exercise
  let cost = costPath toCost arrowsFrom v1 v2

  -- verify
  case minPath arrowsFrom v1 v2 of
    Just w -> cost === toCost w
    Nothing -> cost === Infinity

tests :: TestTree
tests =
  testGroup
    "Graph.PathTest"
    [ testProperty "reflexive" prop_reflexive,
      testProperty "transitive" prop_transitive,
      testProperty "cost" prop_cost,
      testGroup
        "shortest path"
        [ testCase "1 -> 1" $ shortest (Vertex 1) (Vertex 1) @?= Just 0,
          testCase "1 -> 2" $ shortest (Vertex 1) (Vertex 2) @?= Just 1,
          testCase "1 -> 3" $ shortest (Vertex 1) (Vertex 3) @?= Just 1,
          testCase "1 -> 4" $ shortest (Vertex 1) (Vertex 4) @?= Just 2,
          testCase "3 -> 9" $ shortest (Vertex 3) (Vertex 9) @?= Nothing,
          testCase "1 -> 5" $ shortest (Vertex 1) (Vertex 5) @?= Nothing,
          testCase "2 -> 5" $ shortest (Vertex 2) (Vertex 5) @?= Nothing,
          testCase "3 -> 7" $ shortest (Vertex 3) (Vertex 7) @?= Nothing
        ]
    ]
