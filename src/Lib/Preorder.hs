module Lib.Preorder
  ( Preorder (..),
    BoolPO (..),
    CharPO (..),
    CharSetPO (..),
    IntPO (..),
    IntSetPO (..),
    connections,
    (=~),
  )
where

import Ch1.Set (isSubsetOf)
import Control.Monad (guard)

class Preorder a where
  lte :: a -> a -> Bool

-- | determines whether elements are isomorphic
(=~) :: Preorder a => a -> a -> Bool
x =~ y = x `lte` y && y `lte` x

-- | connections between pairs of elements
connections :: Preorder a => [a] -> [(a, a)]
connections xs = do
  x <- xs
  y <- xs
  guard $ x `lte` y
  return (x, y)

newtype IntPO = IntPO Int deriving (Show, Eq, Ord)

instance Preorder IntPO where
  lte = (<=)

newtype CharPO = CharPO Char deriving (Show, Eq, Ord)

instance Preorder CharPO where
  lte = (<=)

newtype BoolPO = BoolPO Bool deriving (Show, Eq, Ord)

instance Preorder BoolPO where
  lte x y = x <= y

newtype IntSetPO = IntSetPO [Int] deriving (Show, Eq, Ord)

instance Preorder IntSetPO where
  lte (IntSetPO x) (IntSetPO y) = x `isSubsetOf` y

newtype CharSetPO = CharSetPO [CharPO] deriving (Show, Eq, Ord)

instance Preorder CharSetPO where
  lte (CharSetPO x) (CharSetPO y) = x `isSubsetOf` y
