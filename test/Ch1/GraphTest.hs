module Ch1.GraphTest (tests) where

import Ch1.Graph
import Control.Monad (guard)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype Vertex = Vertex Int deriving (Eq, Ord, Show)

newtype Arrow = Arrow (Vertex, Vertex)

instance Graph Vertex Arrow where
  vertices = fmap Vertex [1 .. 30]
  arrowsFrom v1@(Vertex x) = do
    v2@(Vertex y) <- vertices
    guard $ (x `mod` 5 - y `mod` 3) == 1
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

  let v1v2 = path v1 v2
      v1v3 = path v1 v3
      v2v3 = path v2 v3

  cover 10 "v1 -> v2 -> v3" $ v1v2 && v2v3
  cover 10 "not v1 -> v3" $ not $ v1v3

  -- exercise and verify
  v1v2 && v2v3 ==> v1v3
  not v1v3 ==> not v1v2 || not v2v3

tests :: TestTree
tests =
  testGroup
    "Ch1.GraphTest"
    [ testProperty "reflexive" prop_reflexive,
      testProperty "transitive" prop_transitive
    ]
