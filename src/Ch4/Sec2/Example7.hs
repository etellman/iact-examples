module Ch4.Sec2.Example7
  ( X (..),
    Y (..),
    XY (..),
    reachable,
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

xEdges :: Matrix BoolWeight
xEdges =
  fromLists $
    (fmap . fmap)
      BoolWeight
      -- N S E W
      [ [True, False, False, False],
        [False, True, True, True],
        [True, False, True, False],
        [True, False, False, True]
      ]

xDistance :: X -> X -> BoolWeight
xDistance = distanceFunc xEdges xOrd xOrd

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

yEdges :: Matrix BoolWeight
yEdges =
  fromLists $
    (fmap . fmap)
      BoolWeight
      [ [True, False, False, False, False],
        [True, True, False, True, False],
        [True, False, True, False, False],
        [False, False, False, True, False],
        [False, False, False, False, True]
      ]

yDistance :: Y -> Y -> BoolWeight
yDistance = distanceFunc yEdges yOrd yOrd

instance PartialOrd Y where
  x <= y =
    let BoolWeight w = yDistance x y
     in w

instance VCategory Y PartialOrdAll where
  hom y y' = PartialOrdAll $ All (y PO.<= y')

data XY = XY !X !Y deriving (Show, Eq)

instance VCategory XY PartialOrdAll where
  hom (XY x y) (XY x' y') = hom x' x <> hom y y'

-- | is y reachable from x
reachable :: X -> Y -> Bool
reachable x y =
  let bridges =
        fromLists $
          (fmap . fmap)
            BoolWeight
            -- N S E W
            [ [False, False, True, False, True],
              [True, False, False, False, True],
              [False, True, False, False, True],
              [False, False, False, False, False]
            ]
      xyDstances = (distances xEdges) `quantMult` bridges `quantMult` (distances yEdges)
      (BoolWeight d) = getElem (xOrd x) (yOrd y) xyDstances
   in d
