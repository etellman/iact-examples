module Ch4.Sec2.Example7
  ( X (..),
    Y (..),
    XY (..),
    reachable,
    connected,
  )
where

import Data.Monoid (All (..))
import Data.PartialOrd as PO
import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (PartialOrdAll (..))

data X = North | South | East | West deriving (Show, Eq)

instance PartialOrd X where
  South <= East = True
  South <= West = True
  West <= North = True
  East <= North = True
  South <= North = True
  x <= x' = x Prelude.== x'

instance VCategory X PartialOrdAll where
  hom x x' = PartialOrdAll $ All (x PO.<= x')

data Y = A | B | C | D | E deriving (Show, Eq)

instance PartialOrd Y where
  B <= D = True
  B <= A = True
  C <= A = True
  y <= y' = y Prelude.== y'

-- | Y opposite
instance VCategory Y PartialOrdAll where
  hom y y' = PartialOrdAll $ All (y PO.<= y')

data XY = XY !X !Y deriving (Show, Eq)

instance VCategory XY PartialOrdAll where
  hom (XY x y) (XY x' y') = hom x' x <> hom y y'

connected :: X -> Y -> PartialOrdAll
connected South A = PartialOrdAll . All $ True
connected East B = PartialOrdAll . All $ True
connected North C = PartialOrdAll . All $ True
connected North E = PartialOrdAll . All $ True
connected _ _ = PartialOrdAll . All $ False

-- | is there a path from x' -> x -> y' -> y?
reachable :: X -> X -> Y -> Y  -> PartialOrdAll
reachable x x' y' y = hom x x' <> connected x' y' <> hom y' y
