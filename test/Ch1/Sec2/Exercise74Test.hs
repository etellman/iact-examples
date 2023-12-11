module Ch1.Sec2.Exercise74Test (tests) where

import Ch1.Set (isSubsetOf)
import Ch1.UpperSet (upperSets)
import Control.Monad (guard)
import Data.Set (toList)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Preorder
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

newtype P = P Int deriving (Show, Eq, Ord)

instance Preorder P where
  lte = (<=)

newtype Q = Q Int deriving (Show, Eq, Ord)

instance Preorder Q where
  lte = (<=)

-- newtype PSetPO = PSetPO [P] deriving (Show, Eq, Ord)

-- instance Preorder PSetPO where
--   lte (PSetPO x) (PSetPO y)= x `isSubsetOf` y

-- newtype QSetPO = QSetPO [P] deriving (Show, Eq, Ord)

-- instance Preorder QSetPO where
--   lte (QSetPO x) (QSetPO y)= x `isSubsetOf` y

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
      pxs = upperSets ps
      qxs = upperSets $ fmap f ps

  us1 <- forAll $ Gen.element qxs
  us2 <- forAll $ Gen.element qxs

  fmap fstar qxs === pxs
  us1 `isSubsetOf` us2 ==> fstar us1 `isSubsetOf` fstar us2

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec2.Exercise74Test"
    [ testProperty "pullback" $ prop_ex74 fstar1,
      testProperty "Boolean map" $ prop_ex74 fstar2
    ]
