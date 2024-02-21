module Ch3.Sec3.Definition41Test (tests) where

import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

newtype F a = F {fromF :: a} deriving (Eq, Show)

instance Functor F where
  fmap f (F a) = F (f a)

newtype G a = G {fromG :: a} deriving (Eq, Show)

instance Functor G where
  fmap f (G a) = G (f a)

fToG :: F a -> G a
fToG (F x) = G x

prop_natural :: Property
prop_natural = property $ do
  -- set up
  fx <- forAll $ F <$> Gen.int (Range.constant (-1000) 1000)
  n <- forAll $ Gen.int (Range.constant (-1000) 1000)
  let f = (+ n)

  -- exercise and verify
  (fToG . fmap f) fx === (fmap f . fToG) fx

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec3.Definition41Test"
    [ testProperty "part B" prop_natural
    ]
