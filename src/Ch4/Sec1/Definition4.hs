module Ch4.Sec1.Definition4
  ( X (..),
    Y (..),
    XY (..),
  )
where

import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (BooleanAnd (..))

newtype X = X Int deriving (Show, Eq, Ord)

instance VCategory X BooleanAnd where
  hom (X a) (X b) = BooleanAnd $ a <= b

newtype Y = Y Int deriving (Show, Eq, Ord)

instance VCategory Y BooleanAnd where
  hom (Y a) (Y b) = BooleanAnd $ a <= b

data XY = XY !X !Y deriving (Show, Eq, Ord)

instance VCategory XY BooleanAnd where
  hom (XY x y) (XY x' y') = hom x x' <> hom y y'
