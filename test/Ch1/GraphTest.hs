module Ch1.GraphTest (tests) where

import Ch1.Graph
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype Vertex = Vertex Char deriving (Eq, Ord, Show)

newtype Arrow = Arrow (Vertex, Vertex)

instance Graph Vertex Arrow where
  vertices = fmap Vertex ['a' .. 'f']
  arrowsFrom v1 = do
    v2 <- filter (v1 <=) vertices
    return $ Arrow (v1, v2)

  source (Arrow (v, _)) = v
  target (Arrow (_, v)) = v
  weight _ = 1

genVertex :: Gen Vertex
genVertex = Gen.element (vertices :: [Vertex])

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

tests :: TestTree
tests =
  testGroup
    "Ch1.GraphTest"
    [ testProperty "reflexive" prop_reflexive,
      testProperty "transitive" prop_transitive
    ]
