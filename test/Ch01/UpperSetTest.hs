module Ch01.UpperSetTest (tests) where

import Ch01.UpperSet
import Control.Monad (filterM)
import Data.List (nub)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_upperSets :: (Show a, Eq a) => [[a]] -> (a -> a -> Bool) -> Property
prop_upperSets xss gte = property $ do
  -- set up
  let elements = (nub . concat) xss
  set <- forAll $ Gen.element $ filter (not . null) xss

  x <- forAll $ Gen.element elements
  y <- forAll $ Gen.element set

  -- exercise and verify
  y `gte` x ==> y `elem` set

tests :: TestTree
tests =
  testGroup
    "Ch01.UpperSetTest"
    [ testGroup
        "Binary preorder"
        [ testProperty "all upper sets" $ do
            let xss = [[True], [False, True]]
            prop_upperSets xss (>=),
          testCase "filter upper sets" $ do
            let xss = filterM (const [False, True]) [False, True]
            upperSets (>=) xss @?= [[], [True], [False, True]]
        ],
      testGroup
        "discrete preorder"
        [ testProperty "all upper sets" $ do
            let xss = filterM (const [False, True]) [1 .. 10] :: [[Int]]
            prop_upperSets xss (\_ _ -> False),
          testCase "filter upper sets" $ do
            let xss = filterM (const [False, True]) [1 .. 3] :: [[Int]]
            upperSets (\_ _ -> False) xss @?= xss
        ]
    ]
