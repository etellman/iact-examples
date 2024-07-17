module Ch4.Sec2.Example7
  ( X (..),
    Y (..),
    XY (..),
    reachable,
    connected,
    xDistance,
    yDistance,
  )
where

import Data.Matrix
import Data.Monoid (All (..))
import Data.PartialOrd as PO
import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (PartialOrdAll (..))
import Preorder.Quantale

data X = North | South | East | West deriving (Show, Eq)

xOrd :: X -> Int
xOrd North = 1
xOrd South = 2
xOrd East = 3
xOrd West = 4

xDistance :: X -> X -> BoolWeight
xDistance =
  let costs =
        fromLists $
          (fmap . fmap)
            BoolWeight
            -- N S E W
            [ [True, False, False, False],
              [False, True, True, True],
              [True, False, True, False],
              [True, False, False, True]
            ]
   in distanceFunc costs xOrd

instance PartialOrd X where
  x <= y =
    let BoolWeight w = xDistance x y
     in w

instance VCategory X PartialOrdAll where
  hom x x' = PartialOrdAll $ All (x PO.<= x')

data Y = A | B | C | D | E deriving (Show, Eq)

yOrd :: Y -> Int
yOrd A = 1
yOrd B = 2
yOrd C = 3
yOrd D = 4
yOrd E = 5

yDistance :: Y -> Y -> BoolWeight
yDistance =
  let costs =
        fromLists $
          (fmap . fmap)
            BoolWeight
            [ [True, False, False, False, False],
              [True, True, False, True, False],
              [True, False, True, False, False],
              [False, False, False, True, False],
              [False, False, False, False, True]
            ]
   in distanceFunc costs yOrd

instance PartialOrd Y where
  x <= y =
    let BoolWeight w = yDistance x y
     in w

-- | Y opposite
instance VCategory Y PartialOrdAll where
  hom y y' = PartialOrdAll $ All (y PO.<= y')

data XY = XY !X !Y deriving (Show, Eq)

instance VCategory XY PartialOrdAll where
  hom (XY x y) (XY x' y') = hom x' x <> hom y y'

connected :: X -> Y -> PartialOrdAll
connected x y =
  let c South A = True
      c East B = True
      c North C = True
      c North E = True
      c _ _ = False
   in (PartialOrdAll . All) $ c x y

-- go from x to y through x' and y'
reachable' :: X -> Y -> X -> Y -> Bool
reachable' x y x' y' =
  let (PartialOrdAll (All z)) = hom x x' <> connected x' y' <> hom y' y
   in z

-- | is there a path from x -> y?
reachable :: X -> Y -> Bool
reachable x y =
  let r = reachable' x y
   in r South A || r East B || r North C || r North E
