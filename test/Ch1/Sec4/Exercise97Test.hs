module Ch1.Sec4.Exercise97Test (tests) where

import Ch1.Partition
  ( partitions,
    samePartition,
  )
import Ch1.Set
  ( closureBy,
    overlapsBy,
    sameElementsBy,
  )
import Data.List (nub)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions ((==>))

newtype S = S Int deriving (Show, Eq, Ord)

newtype T = T Int deriving (Show, Eq, Ord)

checkRightAdjunct :: [S] -> [[T]] -> (S -> T) -> PropertyT IO ()
checkRightAdjunct ss tss g = do
  -- exercise
  let ssFor t = filter (\s -> g s == t) ss
      sss = concat $ (fmap . fmap) ssFor tss

  -- verify
  H.assert $ sameElementsBy (==) (concat sss) ss
  overlapsBy (==) sss === []

  s1 <- forAll $ Gen.element ss
  s2 <- forAll $ Gen.element ss
  cover 20 "same S partition" (samePartition sss s1 s2)

  (samePartition sss s1 s2) ==> samePartition tss (g s1) (g s2)

checkLeftAdjunct :: [[S]] -> (S -> T) -> PropertyT IO ()
checkLeftAdjunct sss g = do
  -- exercise
  let tss = closureBy (==) $ (fmap . fmap) g sss
      ss = concat sss

  -- verify
  H.assert $ sameElementsBy (==) (concat tss) (fmap g ss)
  overlapsBy (==) tss === []

  s1 <- forAll $ Gen.element ss
  s2 <- forAll $ Gen.element ss
  cover 20 "same S partition" (samePartition sss s1 s2)

  (samePartition sss s1 s2) ==> samePartition tss (g s1) (g s2)

prop_example96Left :: Property
prop_example96Left = property $ do
  -- set up
  let sss = [[S 1, S 3], [S 2, S 4]]
      g (S 1) = (T 12)
      g (S 2) = (T 12)
      g (S s) = (T s)

  -- exercise
  checkLeftAdjunct sss g

prop_example96Right :: Property
prop_example96Right = property $ do
  -- set up
  let ss = fmap S [1 .. 4]
      g (S 1) = (T 12)
      g (S 2) = (T 12)
      g (S s) = (T s)
      tss = [fmap T [3, 4, 12]]

  -- exercise
  checkRightAdjunct ss tss g

genExercise97Ss :: Gen [S]
genExercise97Ss =
  fmap S
    <$> toList
    <$> Gen.set
      (Range.constant 2 10)
      (Gen.int $ (Range.linear 0 1000))

exercise97g :: PropertyT IO (S -> T)
exercise97g = do
  n <- forAll $ Gen.int (Range.constant 2 20)
  let g (S s) = T (s `rem` n)

  return g

prop_exercise97Left :: Property
prop_exercise97Left = property $ do
  -- set up
  ss <- forAll genExercise97Ss
  sss <- forAll $ Gen.element (partitions ss)
  g <- exercise97g

  -- exercise
  checkLeftAdjunct sss g

prop_exercise97Right :: Property
prop_exercise97Right = property $ do
  -- set up
  ss <- forAll genExercise97Ss
  g <- exercise97g
  tss <- forAll $ Gen.element $ (partitions . nub . fmap g) ss

  -- exercise
  checkRightAdjunct ss tss g

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Exercise97Test"
    [ testGroup
        "example 96"
        [ testProperty "left" prop_example96Left,
          testProperty "right" prop_example96Right
        ],
      testGroup
        "exercise 97"
        [ testProperty "left" prop_exercise97Left,
          testProperty "right" prop_exercise97Right
        ]
    ]
