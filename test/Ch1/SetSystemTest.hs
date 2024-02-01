module Ch1.SetSystemTest (tests) where

import Data.Containers.ListUtils (nubOrd)
import Ch1.BooleanSystem
import Ch1.Joinable
import Ch1.SetSystem
import Data.List (sort)
import qualified Data.PartialOrd as PO
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.Hedgehog

genSystem :: Gen (SetSystem Int)
genSystem =
  SetSystem . fmap toList
    <$> Gen.list
      (Range.constant 0 20)
      (Gen.set (Range.constant 0 10) (Gen.int $ Range.constant 1 20))

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
  let combined = sort . nubOrd $ elements s1 ++ elements s2

  -- exercise
  let joined = join s1 s2

  -- verify
  elements joined === combined
  H.assert $ disjoint joined
  H.assert $ SetSystem (sets s1 ++ sets s2) PO.<= joined

-- the coarsest partition maps everything to 0
tests :: TestTree
tests =
  testGroup
    "Ch1.SetSystemTest"
    [ testCase "disjoint" $ do
        assertBool "no overlap" $ disjoint $ SetSystem [['a', 'b'], ['c', 'd']]
        assertBool "overlap" $ not . disjoint $ SetSystem [['a', 'b'], ['b', 'c']]
        assertBool "singleton" $ disjoint $ SetSystem [['a']]
        assertBool "empty" $ disjoint (SetSystem [[]] :: SetSystem Char),
      testProperty "simplify" prop_simplify,
      testProperty "join" prop_join,
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
        ]
    ]
