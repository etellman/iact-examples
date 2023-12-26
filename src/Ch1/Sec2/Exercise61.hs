module Ch1.Sec2.Exercise61
  ( Ex61 (..),
    Ex61Set (..),
    CharSetPO (..),
    Ex61Op (..),
    Ex61OpSet (..),
  )
where

import Ch1.Set (isSubsetOf)
import Lib.Preorder as PO

newtype Ex61 = Ex61 Char deriving (Show, Eq, Ord)

instance Preorder Ex61 where
  (Ex61 'a') <= (Ex61 'b') = True
  (Ex61 'a') <= (Ex61 'c') = True
  x <= y = x == y

newtype Ex61Set = Ex61Set [Ex61] deriving (Show, Eq, Ord)

instance Preorder Ex61Set where
  (Ex61Set xs) <= (Ex61Set ys) = xs `isSubsetOf` ys

newtype Ex61Op = Ex61Op Ex61 deriving (Show, Eq, Ord)

instance Preorder Ex61Op where
  (Ex61Op x) <= (Ex61Op y) = y PO.<= x

newtype Ex61OpSet = Ex61OpSet [Ex61Op] deriving (Show, Eq, Ord)

instance Preorder Ex61OpSet where
  (Ex61OpSet xs) <= (Ex61OpSet ys) = xs `isSubsetOf` ys

newtype CharSetPO = CharSetPO [CharPO] deriving (Show, Eq)

instance Preorder CharSetPO where
  (CharSetPO x) <= (CharSetPO y) = x `isSubsetOf` y
