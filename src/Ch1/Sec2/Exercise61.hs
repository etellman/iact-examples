module Ch1.Sec2.Exercise61
  ( Ex52 (..),
    allEx52s,
    Ex52Set (..),
    CharSetPO (..),
    Ex52Op (..),
    Ex52OpSet (..),
  )
where

import Ch1.Set (isSubsetOf)
import Data.PartialOrd as PO
import Preorder.Preorders (CharPO)

data Ex52 = A | B | C deriving (Show, Eq, Ord)

allEx52s :: [Ex52]
allEx52s = [A, B, C]

instance PartialOrd Ex52 where
  A <= B = True
  A <= C = True
  x <= y = x Prelude.== y

newtype Ex52Set = Ex52Set [Ex52] deriving (Show, Eq, Ord)

instance PartialOrd Ex52Set where
  (Ex52Set xs) <= (Ex52Set ys) = xs `isSubsetOf` ys

newtype Ex52Op = Ex52Op Ex52 deriving (Show, Eq, Ord)

instance PartialOrd Ex52Op where
  (Ex52Op x) <= (Ex52Op y) = y PO.<= x

newtype Ex52OpSet = Ex52OpSet [Ex52Op] deriving (Show, Eq, Ord)

instance PartialOrd Ex52OpSet where
  (Ex52OpSet xs) <= (Ex52OpSet ys) = xs `isSubsetOf` ys

newtype CharSetPO = CharSetPO [CharPO] deriving (Show, Eq)

instance PartialOrd CharSetPO where
  (CharSetPO x) <= (CharSetPO y) = x `isSubsetOf` y
