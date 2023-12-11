module Ch1.UpperSetTest (tests) where

import Ch1.Set (powerSet)
import Ch1.UpperSet
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.Preorder
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_upperSets :: (Preorder a, Show a, Eq a) => [a] -> [[a]] -> Property
prop_upperSets elements xss = property $ do
  -- set up
  set <- forAll $ Gen.element $ filter (not . null) xss

  x <- forAll $ Gen.element elements
  y <- forAll $ Gen.element set

  -- exercise and verify
  y `lte` x ==> y `elem` set

tests :: TestTree
tests =
  testGroup
    "Ch1.UpperSetTest"
    [ testGroup
        "Binary preorder"
        [ testProperty "all upper sets" $ do
            let xss = [[], [True], [False, True]]
            prop_upperSets (fmap BoolPO [False, True]) ((fmap . fmap) BoolPO xss),
          testCase "filter upper sets" $ do
            let xs = fmap BoolPO [False, True]
            upperSets xs @?= (fmap . fmap) BoolPO [[], [True], [False, True]]
        ]
        -- testGroup
        --   "discrete preorder"
        --   [ testProperty "all upper sets" $ do
        --       let xs = [1 .. 10] :: [Int]
        --       prop_upperSets (Preorder (==) xs) (powerSet xs),
        --     testCase "filter upper sets" $ do
        --       let xs = [1 .. 3] :: [Int]
        --       upperSets (Preorder (==) xs) @?= powerSet xs
        --   ]
        -- testGroup
        --   "upper set preorder"
        --   [ testCase "1..3" $ do
        --       let xs = [1 .. 3] :: [Int]
        --           usxs = upperSetPreorder (Preorder (<=) xs)
        --       usxs @?= [[], [3], [2, 3], [1, 2, 3]]
        --   ]
    ]
