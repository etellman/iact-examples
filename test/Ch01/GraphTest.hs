module Ch01.GraphTest (tests) where

import Ch01.Graph
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions

g32 :: Graph Int Char
g32 =
  let s 'a' = 1 :: Int
      s 'b' = 1
      s 'c' = 1
      s 'e' = 2
      s 'd' = 2
      s _ = undefined

      t 'a' = 2
      t 'b' = 3
      t 'c' = 3
      t 'd' = 2
      t 'e' = 3
      t _ = undefined
   in Graph [1, 2, 3] ['a' .. 'e'] s t

genGraph :: Gen (Graph Int Int)
genGraph = undefined

prop_reflexive :: Property
prop_reflexive = property $ do
  -- set up
  v <- forAll $ Gen.element (vertices g32)

  -- exercise and verify
  H.assert $ path g32 v v

prop_transitive :: Property
prop_transitive = property $ do
  -- set up
  v1 <- forAll $ Gen.element (vertices g32)
  v2 <- forAll $ Gen.element (vertices g32)
  v3 <- forAll $ Gen.element (vertices g32)

  -- exercise and verify
  path g32 v1 v2 && path g32 v2 v3 ==> path g32 v1 v3

tests :: TestTree
tests =
  testGroup
    "Ch01.GraphTest"
    [ testCase "compose" $ do
        compose g32 'a' 'e' @?= Just 3
        compose g32 'a' 'b' @?= Nothing,
      testCase "connections" $ do
        connections g32 1 2 @?= ['a']
        connections g32 1 3 @?= ['b', 'c']
        connections g32 2 2 @?= ['d']
        connections g32 2 3 @?= ['e']
        connections g32 2 2 @?= ['d']

        assertBool "3 -> 4" $ null (connections g32 3 4)
        assertBool "1 -> 1" $ null (connections g32 1 1),
      testCase "paths" $ do
        assertBool "1 -> 2" $ path g32 1 2
        assertBool "1 -> 1" $ path g32 1 1
        assertBool "1 -> 3" $ path g32 1 3
        assertBool "2 -> 3" $ path g32 2 3
        assertBool "3 -> 2" $ not $ path g32 3 2
        assertBool "1 -> 4" $ not $ path g32 1 4,
      testGroup
        "graph as partial order"
        [ testProperty "reflexive property" prop_reflexive,
          testProperty "transitive property" prop_transitive
        ]
    ]
