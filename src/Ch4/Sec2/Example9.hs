module Ch4.Sec2.Example9
  ( X (..),
    Y (..),
    XY (..),
    cost,
  )
where

import Data.Matrix
import Lib.VCategory (VCategory (..))
import Monoid.Cost
import Preorder.Quantale

data X = A | B | C | D deriving (Show, Eq)

xOrd :: X -> Int
xOrd A = 1
xOrd B = 2
xOrd C = 3
xOrd D = 4

xEdges :: Matrix IntCost
xEdges =
  fromLists $
    (fmap . fmap)
      IntCost
      [ [Cost 0, Infinity, Cost 3, Infinity],
        [Cost 2, Cost 0, Infinity, Cost 5],
        [Infinity, Cost 3, Cost 0, Infinity],
        [Infinity, Infinity, Cost 4, Cost 0]
      ]

xDistance :: X -> X -> IntCost
xDistance = distanceFunc xEdges xOrd xOrd

instance VCategory X IntCost where
  hom = xDistance

data Y = X | Y | Z deriving (Show, Eq)

yOrd :: Y -> Int
yOrd X = 1
yOrd Y = 2
yOrd Z = 3

yEdges :: Matrix IntCost
yEdges =
  fromLists $
    (fmap . fmap)
      IntCost
      [ [Cost 0, Cost 4, Cost 3],
        [Cost 3, Cost 0, Infinity],
        [Infinity, Cost 3, Cost 0]
      ]

yDistance :: Y -> Y -> IntCost
yDistance = distanceFunc yEdges yOrd yOrd

instance VCategory Y IntCost where
  hom = yDistance

data XY = XY !X !Y deriving (Show, Eq)

instance VCategory XY IntCost where
  hom (XY x y) (XY x' y') = hom x' x <> hom y y'

-- | what is the minimum cost to go from x to y?
cost :: X -> Y -> Cost Int
cost x y =
  let bridges =
        fromLists $
          (fmap . fmap)
            IntCost
            [ [Infinity, Infinity, Infinity],
              [Cost 11, Infinity, Infinity],
              [Infinity, Infinity, Infinity],
              [Infinity, Cost 9, Infinity]
            ]
      xyDstances = (distances xEdges) `quantMult` bridges `quantMult` (distances yEdges)
      (IntCost d) = getElem (xOrd x) (yOrd y) xyDstances
   in d
