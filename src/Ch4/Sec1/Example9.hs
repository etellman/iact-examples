module Ch4.Sec1.Example9
  ( X (..),
    Y (..),
    XY (..),
    cost,
    bridge,
  )
where

-- import Data.Monoid (Sum (..))
import Lib.VCategory (VCategory (..))
import Monoid.Cost

data X = A | B | C | D deriving (Show, Eq)

instance VCategory X Cost where
  hom A B = Cost 6
  hom A C = Cost 3
  hom A D = Cost 11
  hom B A = Cost 2
  hom B C = Cost 5
  hom B D = Cost 5
  hom C A = Cost 5
  hom C B = Cost 3
  hom C D = Cost 8
  hom D A = Cost 9
  hom D B = Cost 7
  hom D C = Cost 4
  hom _ _ = Cost 0

data Y = X | Y | Z deriving (Show, Eq)

instance VCategory Y Cost where
  hom X Y = Cost 4
  hom X Z = Cost 3
  hom Y X = Cost 3
  hom Y Z = Cost 6
  hom Z X = Cost 7
  hom Z Y = Cost 4
  hom _ _ = Cost 0

data XY = XY !X !Y deriving (Show, Eq)

instance VCategory XY Cost where
  hom (XY x y) (XY x' y') = hom x' x <> hom y y'

bridge :: X -> Y -> Cost
bridge B X = Cost 11
bridge D Y = Cost 9
bridge _ _ = Infinity

-- | what is the cost to go from x -> x' -> y' -> y?
cost :: X -> X -> Y -> Y -> Cost
cost x x' y' y = hom x x' <> bridge x' y' <> hom y' y
