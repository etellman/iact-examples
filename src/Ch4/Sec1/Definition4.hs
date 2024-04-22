module Ch4.Sec1.Definition4
  ( X (..),
    Y (..),
    XY (..),
  )
where

import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (BooleanAnd (..))
import Data.PartialOrd as PO

newtype X = X Int deriving (Show, Eq, Ord)

instance PartialOrd X where
  X x <= X x' = x Prelude.<= x'

instance VCategory X BooleanAnd where
  hom x x' = BooleanAnd $ x PO.<= x'

newtype Y = Y Int deriving (Show, Eq, Ord)

instance PartialOrd Y where
  Y y <= Y y' = y Prelude.<= y'

instance VCategory Y BooleanAnd where
  hom y y' = BooleanAnd $ y PO.<= y'

data XY = XY !X !Y deriving (Show, Eq, Ord)

instance VCategory XY BooleanAnd where
  hom (XY x y) (XY x' y') = hom x x' <> hom y y'
