module Preorder.QuantaleTest (tests) where

import Data.Matrix
import Gen.Cost
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Monoid.Cost
import Preorder.Quantale
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_multiplyFinite :: Property
prop_multiplyFinite = property $ do
  -- set up
  v1 <- forAll $ Gen.int $ Range.linear 1 1000
  v2 <- forAll $ Gen.int $ Range.linear 1 1000

  -- exercise
  let p = (EdgeCost $ Cost v1) * (EdgeCost $ Cost v2)

  -- verify
  p === EdgeCost (Cost (v1 + v2))

prop_multiplyInfinite :: Property
prop_multiplyInfinite = property $ do
  -- set up
  c <- forAll $ EdgeCost <$> genCost (Range.linear 1 1000)

  -- exercise
  let p = (EdgeCost Infinity) * c

  -- verify
  p === EdgeCost Infinity

prop_plusInfinity :: Property
prop_plusInfinity = property $ do
  -- set up
  c <- forAll $ EdgeCost <$> genCost (Range.linear 1 1000)

  -- exercise and verify
  c + (EdgeCost Infinity) === c

prop_plusPositive :: Property
prop_plusPositive = property $ do
  -- mi
  v1 <- forAll $ Gen.int $ Range.linear 1 100
  v2 <- forAll $ Gen.int $ Range.linear (v1 + 1) 200

  -- exercise and verify
  (EdgeCost $ Cost v1) + (EdgeCost $ Cost v2) === EdgeCost (Cost v1)

prop_plusZero :: Property
prop_plusZero = property $ do
  -- mi
  c <- forAll $ EdgeCost <$> genCost (Range.linear 0 1000)
  let z = EdgeCost $ Cost 0

  -- exercise and verify
  z + c === c
  c + z === c

tests :: TestTree
tests =
  testGroup
    "Preorder.QuantaleTest"
    [ testCase "multiply" $
        do
          let x =
                fromLists $
                  (fmap . fmap)
                    EdgeCost
                    [ [Infinity, Infinity],
                      [Infinity, 1],
                      [1, 1]
                    ]
              y =
                fromLists $
                  (fmap . fmap)
                    EdgeCost
                    [ [1, 1, Infinity],
                      [1, Infinity, 1]
                    ]
              z =
                fromLists $
                  (fmap . fmap)
                    EdgeCost
                    [ [Infinity, Infinity, Infinity],
                      [2, Infinity, 2],
                      [2, 2, 2]
                    ]
          multStd x y @?= z,
      testGroup
        "EdgeCost"
        [ testGroup
            "plus"
            [ testProperty "infinite" prop_plusInfinity,
              testProperty "finite" prop_plusPositive,
              testProperty "zero" prop_plusZero,
              testCase "0 + infinity" $ EdgeCost (Cost 0) + EdgeCost Infinity @?= EdgeCost Infinity,
              testCase "infinity + 0" $ EdgeCost Infinity + EdgeCost (Cost 0) @?= EdgeCost Infinity
            ],
          testGroup
            "multiply"
            [ testProperty "finite" prop_multiplyFinite,
              testProperty "infinite" prop_multiplyInfinite
            ]
        ]
    ]
