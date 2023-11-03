module Ch01.UpperSetTest (tests) where

import Control.Monad (filterM)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_upperSets :: (Show a, Eq a) => [a] -> [[a]] -> (a -> a -> Bool) -> Property
prop_upperSets elems upperSets gte = property $ do
  -- set up
  xs <- forAll $ Gen.element $ filter (not . null) upperSets
  x <- forAll $ Gen.element xs
  y <- forAll $ Gen.element elems

  -- exercise and verify
  y `gte` x ==> y `elem` xs

tests :: TestTree
tests =
  testGroup
    "Ch01.UpperSetTest"
    [ testProperty "Boolean" $ do
        let elems = [False, True]
            upperSets = [[True], [False, True]]
        prop_upperSets elems upperSets (>=),
      testProperty "discrete preorder" $ do
        let elems = [1 .. 10] :: [Int]
            upperSets = filterM (const [False, True]) elems
        prop_upperSets elems upperSets (\_ _ -> False)
    ]
