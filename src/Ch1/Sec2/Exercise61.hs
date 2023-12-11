module Ch1.Sec2.Exercise61
  ( Ex61 (..),
    Ex61Set (..),
    Ex61Op (..),
    Ex61OpSet (..),
  )
where

import Ch1.Set (isSubsetOf)
import Lib.Preorder

newtype Ex61 = Ex61 Char deriving (Show, Eq, Ord)

instance Preorder Ex61 where
  lte (Ex61 'a') (Ex61 'b') = True
  lte (Ex61 'a') (Ex61 'c') = True
  lte x y = x == y

newtype Ex61Set = Ex61Set [Ex61] deriving (Show, Eq, Ord)

instance Preorder Ex61Set where
  lte (Ex61Set xs) (Ex61Set ys) = xs `isSubsetOf` ys

newtype Ex61Op = Ex61Op Ex61 deriving (Show, Eq, Ord)

instance Preorder Ex61Op where
  lte (Ex61Op x) (Ex61Op y) = y `lte` x

newtype Ex61OpSet = Ex61OpSet [Ex61Op] deriving (Show, Eq, Ord)

instance Preorder Ex61OpSet where
  lte (Ex61OpSet xs) (Ex61OpSet ys) = xs `isSubsetOf` ys
