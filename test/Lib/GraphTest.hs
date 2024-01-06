module Lib.GraphTest (tests) where

import Control.Monad (guard)
import Data.Monoid (Sum (..))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.Graph
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype Vertex = Vertex Int deriving (Eq, Ord, Show)

newtype TestArrow = TestArrow (Vertex, Vertex)

instance Arrow TestArrow Vertex IntWeight where
  source (TestArrow (v, _)) = v
  target (TestArrow (_, v)) = v
  weight' = const $ IntWeight (Sum 1)

vertices :: [Vertex]
vertices = fmap Vertex [1 .. 9]

arrowsFrom :: Vertex -> [TestArrow]
arrowsFrom v1@(Vertex x) = do
  i <- [0, 1]
  let y = (2 * x + i) `mod` 5
  guard $ y /= 0
  return $ TestArrow (v1, Vertex y)

genVertex :: Gen Vertex
genVertex = Gen.element (vertices :: [Vertex])

prop_reflexive :: Property
prop_reflexive = property $ do
  -- set up
  v <- forAll genVertex

  -- exercise and verify
  H.assert $ isPath arrowsFrom v v

prop_transitive :: Property
prop_transitive = property $ do
  -- set up
  v1 <- forAll genVertex
  v2 <- forAll genVertex
  v3 <- forAll genVertex


  let ip = isPath arrowsFrom
      v1v2 = ip v1 v2
      v1v3 = ip v1 v3
      v2v3 = ip v2 v3

  cover 10 "indirect route" $ v1v2 && v2v3
  cover 5 "direct and no indirect route" $ (not v1v2 || not v2v3) && v1v3
  cover 5 "no route" $ not $ v1v3

  -- exercise and verify
  v1v2 && v2v3 ==> v1v3
  not v1v3 ==> not v1v2 || not v2v3

weightToInt :: IntWeight -> Int
weightToInt (IntWeight (Sum x)) = x

shortest :: Vertex -> Vertex -> Maybe Int
shortest v1 v2 =
  let w = minPath arrowsFrom v1 v2
    in fmap weightToInt w

tests :: TestTree
tests =
  testGroup
    "Lib.GraphTest"
    [ testProperty "reflexive" prop_reflexive,
      testProperty "transitive" prop_transitive,
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
