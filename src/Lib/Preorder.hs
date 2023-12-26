module Lib.Preorder
  ( Preorder (..),
    BoolPO (..),
    CharPO (..),
    IntPO (..),
    connections,
    (=~),
  )
where

import Control.Monad (guard)

class Preorder a where
  (<=) :: a -> a -> Bool

-- | determines whether elements are isomorphic
(=~) :: Preorder a => a -> a -> Bool
x =~ y = x Lib.Preorder.<= y && y Lib.Preorder.<= x

-- | connections between pairs of elements
connections :: Preorder a => [a] -> [(a, a)]
connections xs = do
  x <- xs
  y <- xs
  guard $ x Lib.Preorder.<= y
  return (x, y)

newtype IntPO = IntPO Int deriving (Show, Eq, Ord)

instance Preorder IntPO where
  (<=) = (Prelude.<=)

newtype CharPO = CharPO Char deriving (Show, Eq, Ord)

instance Preorder CharPO where
  (<=) = (Prelude.<=)

newtype BoolPO = BoolPO Bool deriving (Show, Eq, Ord)

instance Preorder BoolPO where
  (<=) = (Prelude.<=)
