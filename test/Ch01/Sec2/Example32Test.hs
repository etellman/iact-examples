module Ch01.Sec2.Example32Test (tests) where

import Ch01.Graph
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

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

prop_example_32 :: Property
prop_example_32 = property $ do
  -- set up
  let ss = [('a', [11, 12]), ('b', [13]), ('c', [32]), ('d', [22, 23])] :: [(Char, [Int])]
      f 11 = 'a'
      f 12 = 'a'
      f 13 = 'b'
      f 32 = 'c'
      f 22 = 'd'
      f 23 = 'd'
      f _ = undefined
  x <- forAll $ Gen.element (concat $ fmap snd ss)

  -- exercise
  let xs = lookup (f x) ss

  -- verify
  H.assert $ fmap (elem x) xs == Just True

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec2.Example32Test"
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
      testProperty "Example 32" prop_example_32
    ]
