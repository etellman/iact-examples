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

data X = A | B | C | D deriving (Show, Eq)

instance VCategory X Int where
  hom A B = 6
  hom A C = 3
  hom A D = 11
  hom B A = 2
  hom B C = 5
  hom B D = 5
  hom C A = 5
  hom C B = 3
  hom C D = 8
  hom D A = 9
  hom D B = 7
  hom D C = 4
  hom _ _ = 0

data Y = X | Y | Z deriving (Show, Eq)

instance VCategory Y Int where
  hom X Y = 4
  hom X Z = 3
  hom Y X = 3
  hom Y Z = 6
  hom Z X = 7
  hom Z Y = 4
  hom _ _ = 0

data XY = XY !X !Y deriving (Show, Eq)

instance VCategory XY Int where
  hom (XY x y) (XY x' y') = hom x' x + hom y y'

bridge :: X -> Y -> Int
bridge B X = 11
bridge D Y = 9
bridge _ _ = undefined

-- | what is the cost to go from x -> x' -> y' -> y?
cost :: X -> X -> Y -> Y  -> Int
cost x x' y' y = hom x x' + bridge x' y' + hom y' y
