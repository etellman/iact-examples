module Monoid.CharSetMonoid
  ( CharSetMonoid (..),
    chars,
  )
where

import Ch1.Set (isSubsetOf)
import Data.List (intersect)
import Lib.Preorder as PO

chars :: [Char]
chars = ['a' .. 'd']

newtype CharSetMonoid = CharSetMonoid [Char] deriving (Eq, Ord, Show)

instance Preorder CharSetMonoid where
  (CharSetMonoid xs) <= (CharSetMonoid ys) = xs `isSubsetOf` ys

instance Monoid CharSetMonoid where
  mempty = CharSetMonoid chars

instance Semigroup CharSetMonoid where
  (CharSetMonoid xs) <> (CharSetMonoid ys) = CharSetMonoid (xs `intersect` ys)
