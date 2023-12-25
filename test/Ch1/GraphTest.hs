module Ch1.GraphTest (tests) where

import Ch1.Graph
import Data.Maybe (fromJust)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype Vertex = Vertex Char deriving (Eq, Ord, Show)

instance Graph Vertex where
  vertices = fmap Vertex ['a' .. 'f']
  arrows = do
    v1 <- vertices
    v2 <- filter (v1 <=) vertices
    return (v1, v2)

genVertex :: Gen Vertex
genVertex = Gen.element (vertices :: [Vertex])

genArrow :: Gen (Vertex, Vertex)
genArrow = Gen.element (arrows :: [(Vertex, Vertex)])

genFilteredArrow :: ((Vertex, Vertex) -> Bool) -> Gen (Vertex, Vertex)
genFilteredArrow p = Gen.element (filter p arrows)

prop_reflexive :: Property
prop_reflexive = property $ do
  -- set up
  v <- forAll genVertex

  -- exercise and verify
  H.assert $ path v v

prop_transitive :: Property
prop_transitive = property $ do
  -- set up
  v1 <- forAll genVertex
  v2 <- forAll genVertex
  v3 <- forAll genVertex

  let viaV2 = path v1 v2 && path v2 v3
  cover 10 "path exists" viaV2
  cover 10 "no path" viaV2

  -- exercise and verify
  viaV2 ==> path v1 v3

prop_compose :: Property
prop_compose = property $ do
  -- set up
  a1 <- forAll genArrow
  a2 <- forAll $ genFilteredArrow (\a -> source a == target a1)

  -- exercise
  let composed = fromJust $ compose a1 a2

  -- verify
  (source composed) === source a1
  (target composed) === target a2

prop_notComposable :: Property
prop_notComposable = property $ do
  -- set up
  a1 <- forAll genArrow
  a2 <- forAll $ genFilteredArrow (\a -> source a /= target a1)

  -- exercise
  let composed = compose a1 a2

  -- verify
  composed === Nothing

tests :: TestTree
tests =
  testGroup
    "Ch1.GraphTest"
    [ testGroup
        "graph as partial order"
        [ testProperty "reflexive" prop_reflexive,
          testProperty "transitive" prop_transitive,
          testGroup
            "compose"
            [ testProperty "composable" prop_compose,
              testProperty "not composable" prop_notComposable
            ]
        ]
    ]
