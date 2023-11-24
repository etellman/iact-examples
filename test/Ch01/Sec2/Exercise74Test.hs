module Ch01.Sec2.Exercise74Test (tests) where

import Ch01.Preorder (Preorder (..))
import Ch01.Set (isSubsetOf)
import Ch01.UpperSet (upperSets)
import Control.Monad (guard)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype P = P Int deriving (Show, Eq, Ord)

newtype Q = Q Int deriving (Show, Eq, Ord)

fstar1 :: (P -> Q) -> [P] -> [Q] -> [P]
fstar1 f ps qs = do
  q <- qs
  p <- ps
  guard $ f p == q
  return p

fstar2 :: (P -> Q) -> [P] -> [Q] -> [P]
fstar2 f ps qs = do
  p <- ps
  let u = flip elem qs
  guard $ (u . f) p
  return p

prop_ex74 :: ((P -> Q) -> [P] -> [Q] -> [P]) -> Property
prop_ex74 fstarFor = property $ do
  ps <-
    forAll $
      fmap P
        <$> toList
        <$> Gen.set
          (Range.constant 1 10)
          (Gen.int $ Range.constant 0 100)
  m <- forAll $ Gen.int (Range.constant 2 10)

  let f (P p) = Q (m * p)
      fstar = fstarFor f ps
      Preorder plte pxs = Preorder isSubsetOf (upperSets $ Preorder (<=) ps)
      Preorder qlte qxs = Preorder isSubsetOf (upperSets $ Preorder (<=) (fmap f ps))

  us1 <- forAll $ Gen.element qxs
  us2 <- forAll $ Gen.element qxs

  fmap fstar qxs === pxs
  qlte us1 us2 ==> plte (fstar us1) (fstar us2)

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec2.Exercise74Test"
    [ testProperty "pullback" $ prop_ex74 fstar1,
      testProperty "Boolean map" $ prop_ex74 fstar2
    ]
