module Ch4.Sec2.Example9
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

instance VCategory X IntCost where
  hom A B = IntCost $ Cost 6
  hom A C = IntCost $ Cost 3
  hom A D = IntCost $ Cost 11
  hom B A = IntCost $ Cost 2
  hom B C = IntCost $ Cost 5
  hom B D = IntCost $ Cost 5
  hom C A = IntCost $ Cost 5
  hom C B = IntCost $ Cost 3
  hom C D = IntCost $ Cost 8
  hom D A = IntCost $ Cost 9
  hom D B = IntCost $ Cost 7
  hom D C = IntCost $ Cost 4
  hom _ _ = IntCost $ Cost 0

data Y = X | Y | Z deriving (Show, Eq)

instance VCategory Y IntCost where
  hom X Y = IntCost $ Cost 4
  hom X Z = IntCost $ Cost 3
  hom Y X = IntCost $ Cost 3
  hom Y Z = IntCost $ Cost 6
  hom Z X = IntCost $ Cost 7
  hom Z Y = IntCost $ Cost 4
  hom _ _ = IntCost $ Cost 0

data XY = XY !X !Y deriving (Show, Eq)

instance VCategory XY IntCost where
  hom (XY x y) (XY x' y') = hom x' x <> hom y y'

bridge :: X -> Y -> IntCost
bridge B X = IntCost $ Cost 11
bridge D Y = IntCost $ Cost 9
bridge _ _ = IntCost $ Infinity

-- | what is the cost to go from x -> x' -> y' -> y?
cost :: X -> X -> Y -> Y -> IntCost
cost x x' y' y = hom x x' <> bridge x' y' <> hom y' y
