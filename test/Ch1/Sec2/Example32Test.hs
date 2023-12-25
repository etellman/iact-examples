module Ch1.Sec2.Example32Test (tests) where

-- import Ch1.Graph
-- import Hedgehog as H
-- import qualified Hedgehog.Gen as Gen
import Test.Tasty
-- import Test.Tasty.HUnit
-- import Test.Tasty.Hedgehog

-- newtype Vertex = Vertex Char deriving (Eq, Show)

-- instance Graph Vertex where
--   vertices = fmap Vertex ['a' .. 'f']
--   arrows =
--     [ ('1', '2'),
--       ('1', '3'),
--       ('2', '2'),
--       ('d', '2'),
--     ]

-- prop_example_32 :: Property
-- prop_example_32 = property $ do
--   -- set up
--   let ss = [('a', [11, 12]), ('b', [13]), ('c', [32]), ('d', [22, 23])] :: [(Char, [Int])]
--       f 11 = 'a'
--       f 12 = 'a'
--       f 13 = 'b'
--       f 32 = 'c'
--       f 22 = 'd'
--       f 23 = 'd'
--       f _ = undefined
--   x <- forAll $ Gen.element (concat $ fmap snd ss)

--   -- exercise
--   let xs = lookup (f x) ss

--   -- verify
--   H.assert $ fmap (elem x) xs == Just True

tests :: TestTree
tests = testGroup "Ch1.Sec2.Example32Test" []

-- tests =
--   testGroup
--     "Ch1.Sec2.Example32Test"
--     [ testCase "compose" $ do
--         compose g32 'a' 'e' @?= Just 3
--         compose g32 'a' 'b' @?= Nothing,
--       testCase "connections" $ do
--         connections g32 1 2 @?= ['a']
--         connections g32 1 3 @?= ['b', 'c']
--         connections g32 2 2 @?= ['d']
--         connections g32 2 3 @?= ['e']
--         connections g32 2 2 @?= ['d']

--         assertBool "3 -> 4" $ null (connections g32 3 4)
--         assertBool "1 -> 1" $ null (connections g32 1 1),
--       testCase "paths" $ do
--         assertBool "1 -> 2" $ path g32 1 2
--         assertBool "1 -> 1" $ path g32 1 1
--         assertBool "1 -> 3" $ path g32 1 3
--         assertBool "2 -> 3" $ path g32 2 3
--         assertBool "3 -> 2" $ not $ path g32 3 2
--         assertBool "1 -> 4" $ not $ path g32 1 4,
--       testProperty "Example 32" prop_example_32
--     ]
