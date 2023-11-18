module Ch01.GraphTest (tests) where

import Ch01.Graph
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

-- generate a graph with the indicated number of vertices and arrows
genGraph :: Int -> Int -> Gen (Graph Int Int)
genGraph nv na = do
  let vs = [0 .. nv - 1]

  sources <- Gen.list (Range.singleton na) (Gen.element vs)
  targets <- Gen.list (Range.singleton na) (Gen.element vs)

  return $ Graph vs [0 .. na - 1] (sources !!) (targets !!)

prop_reflexive :: Property
prop_reflexive = property $ do
  -- set up
  nv <- forAll $ Gen.int (Range.linear 1 10)
  na <- forAll $ Gen.int (Range.linear 1 10)

  g <- forAll $ genGraph nv na
  v <- forAll $ Gen.element (vertices g)

  -- exercise and verify
  H.assert $ path g v v

prop_transitive :: Property
prop_transitive = property $ do
  -- set up
  nv <- forAll $ Gen.int (Range.linear 1 20)
  na <- forAll $ Gen.int (Range.linear 1 20)

  g <- forAll $ genGraph nv na

  v1 <- forAll $ Gen.element (vertices g)
  v2 <- forAll $ Gen.element (vertices g)
  v3 <- forAll $ Gen.element (vertices g)

  -- exercise and verify
  path g v1 v2 && path g v2 v3 ==> path g v1 v3

tests :: TestTree
tests =
  testGroup
    "Ch01.GraphTest"
    [ testGroup
        "graph as partial order"
        [ testProperty "reflexive property" prop_reflexive,
          testProperty "transitive property" prop_transitive
        ]
    ]
