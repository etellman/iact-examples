module Ch01.SetSystemTest (tests) where

import Ch01.BooleanSystem
import Ch01.Joinable
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
  let js = simplify s

  -- verify
  elements js === elements s
  H.assert $ disjoint js
  H.assert $ s PO.<= js

prop_join :: Property
prop_join = property $ do
  -- set up
  s1 <- forAll genSystem
  s2 <- forAll genSystem
  let combined = ((sort . nub) $ (elements s1) ++ (elements s2))

  -- exercise
  let js = join s1 s2

  -- verify
  elements js === combined
  H.assert $ disjoint js
  H.assert $ SetSystem (sets s1 ++ sets s2) PO.<= js

prop_exercise1_3 :: Property
prop_exercise1_3 = property $ do
  -- set up
  let ss = partitions ['a' .. 'e']
  a <- forAll $ Gen.element ss
  b <- forAll $ Gen.element ss
  x <- forAll $ Gen.element ss

  -- exercise
  let c = join a b

  -- verify
  H.assert $ a PO.<= c
  H.assert $ b PO.<= c

  -- c is the least system greater than or equal to both a and b
  a PO.<= x && b PO.<= x ==> c PO.<= x

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
      testProperty "exercise 1.3" prop_exercise1_3
    ]
