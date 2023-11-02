module Ch01.SetSystemTest (tests) where

import Ch01.BooleanSystem
import Ch01.Joinable
import Ch01.Set (cartesianProduct)
import Ch01.SetSystem
import Data.List (nub, sort)
import qualified Data.PartialOrd as PO
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.Hedgehog
import TestLib.Assertions

genSystem :: Gen (SetSystem Int)
genSystem =
  SetSystem
    <$> (fmap nub)
    <$> Gen.list
      (Range.constant 0 20)
      (Gen.list (Range.constant 0 10) (Gen.int $ Range.constant 1 20))

prop_simplify :: Property
prop_simplify = property $ do
  -- set up
  s <- forAll genSystem

  -- exercise
  let simplified = simplify s

  -- verify
  elements simplified === elements s
  H.assert $ disjoint simplified
  H.assert $ s PO.<= simplified

prop_join :: Property
prop_join = property $ do
  -- set up
  s1 <- forAll genSystem
  s2 <- forAll genSystem
  let combined = ((sort . nub) $ (elements s1) ++ (elements s2))

  -- exercise
  let joined = join s1 s2

  -- verify
  elements joined === combined
  H.assert $ disjoint joined
  H.assert $ SetSystem (sets s1 ++ sets s2) PO.<= joined

prop_exercise3 :: Property
prop_exercise3 = property $ do
  -- set up
  let systems = partitions ['a' .. 'e']
  system1 <- forAll $ Gen.element systems
  system2 <- forAll $ Gen.element systems

  -- exercise
  let joined = join system1 system2

  -- verify
  H.assert $ system1 PO.<= joined
  H.assert $ system2 PO.<= joined

  system3 <- forAll $ Gen.element systems
  system1 PO.<= system3 && system2 PO.<= system3 ==> joined PO.<= system3

convertIndex :: Eq a => SetSystem a -> SetSystem a -> Int -> Int
convertIndex (SetSystem xs) ySystem i =
  let x = head $ xs !! i
   in partitionFor ySystem x

prop_example47 :: Property
prop_example47 = property $ do
  -- set up
  let systems = partitions ['a' .. 'e']
  system1 <- forAll $ Gen.element systems
  system2 <- forAll $ Gen.element systems

  c <- forAll $ Gen.element ['a' .. 'e']

  let p1 = partitionFor system1
      p2 = partitionFor system2
      p1To2 = convertIndex system1 system2

  -- exercise and verify
  system1 PO.<= system2 ==> (p1To2 . p1) c == p2 c

tests :: TestTree
tests =
  testGroup
    "Ch01.SetSystemTest"
    [ testCase "disjoint" $ do
        assertBool "no overlap" $ disjoint $ SetSystem [['a', 'b'], ['c', 'd']]
        assertBool "overlap" $ not . disjoint $ SetSystem [['a', 'b'], ['b', 'c']]
        assertBool "singleton" $ disjoint $ SetSystem [['a']]
        assertBool "empty" $ disjoint $ (SetSystem [[]] :: SetSystem Char),
      testProperty "simplify" prop_simplify,
      testProperty "join" prop_join,
      testCase "exercise 1.2" $ do
        let s1 = SetSystem [[11, 12], [13], [21], [22, 23]] :: SetSystem Int
            s2 = SetSystem [[11], [21], [12, 22], [13, 23]]
        join s1 s2 @?= SetSystem [[11, 12, 13, 22, 23], [21]],
      testGroup
        "generative properties"
        [ testCase
            "phi doesn't preserve join"
            $ do
              -- set up
              let s1 = SetSystem [['a', 'b'], ['c']]
                  s2 = SetSystem [['a'], ['b', 'c']]
                  phi = connected 'a' 'c'

              -- phi is false for both of the original systems
              assertBool "s1" $ not (phi s1)
              assertBool "s2" $ not (phi s2)

              -- but true for the joined system
              assertBool "s1 v s2" $ phi (join s1 s2),
          testCase
            "phi preserves partial order"
            $ do
              -- set up
              let sA = SetSystem [['a', 'b'], ['c']]
                  sB = SetSystem [['a'], ['b', 'c']]
                  phi = connected 'a' 'c'

                  phiA = BooleanSystem $ phi sA
                  phiB = BooleanSystem $ phi sB

              assertBool "preserve order" $
                join phiA phiB PO.<= BooleanSystem (phi $ join sA sB)
        ],
      testCase "partition" $ do
        let xs = [1 .. 4] :: [Int]
        (length $ partitions xs) @?= 15,
      testGroup
        "distribute"
        [ testCase "non-empty" $
            distribute 'a' ["b", "c", "de"]
              @=? [["ab", "c", "de"], ["b", "ac", "de"], ["b", "c", "ade"], ["b", "c", "de", "a"]],
          testCase "empty" $ distribute 'a' [] @=? [["a"]],
          testCase "bind" $
            ([["bc", "e"], ["d"]] >>= distribute 'a')
              @?= [["abc", "e"], ["bc", "ae"], ["bc", "e", "a"], ["ad"], ["d", "a"]]
        ],
      testProperty "exercise 1.3" prop_exercise3,
      testCase "exercise 1.37" $ do
        -- set up
        let xs = partitions ['a', 'b', 'c']
            pairs = cartesianProduct xs xs

        -- verify
        length (filter (\(x, y) -> x PO.<= y) pairs) @=? 12,
      testProperty "example 1.47" prop_example47
    ]
