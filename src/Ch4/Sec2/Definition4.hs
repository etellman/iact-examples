module Ch4.Sec2.Definition4
  ( X (..),
    Y (..),
    XY (..),
  )
where

import Data.Monoid (All (..))
import Data.PartialOrd as PO
import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (PartialOrdAll (..))

newtype X = X Int deriving (Show, Eq, Ord)

instance PartialOrd X where
  X x <= X x' = x Prelude.<= x'

instance VCategory X PartialOrdAll where
  hom x x' = PartialOrdAll $ All (x PO.<= x')

newtype Y = Y Int deriving (Show, Eq, Ord)

instance PartialOrd Y where
  Y y <= Y y' = y Prelude.<= y'

instance VCategory Y PartialOrdAll where
  hom y y' = PartialOrdAll $ All (y PO.<= y')

data XY = XY !X !Y deriving (Show, Eq, Ord)

instance VCategory XY PartialOrdAll where
  hom (XY x y) (XY x' y') = hom x' x <> hom y y'
