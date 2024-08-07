module Ch4.Sec3.Exercise15
  ( X (..),
    Y (..),
    Z (..),
    xDistance,
    yDistance,
    zDistance,
    phi,
    phiPsi,
  )
where

import Data.Matrix
import Lib.VCategory (VCategory (..))
import Monoid.Cost
import Preorder.Quantale

data X = A | B | C | D deriving (Eq, Show)

xIndex :: X -> Int
xIndex A = 1
xIndex B = 2
xIndex C = 3
xIndex D = 4

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

instance VCategory X IntCost where
  hom = distanceFunc xEdges xIndex xIndex

xDistance :: X -> X -> Cost Int
xDistance x x' = let (IntCost c) = hom x x' in c

data Y = X | Y | Z deriving (Eq, Show)

yIndex :: Y -> Int
yIndex X = 1
yIndex Y = 2
yIndex Z = 3

yEdges :: Matrix IntCost
yEdges =
  fromLists $
    (fmap . fmap)
      IntCost
      [ [Cost 0, Cost 4, Cost 3],
        [Cost 3, Cost 0, Infinity],
        [Infinity, Cost 4, Cost 0]
      ]

instance VCategory Y IntCost where
  hom = distanceFunc yEdges yIndex yIndex

yDistance :: Y -> Y -> Cost Int
yDistance y y' = let (IntCost c) = hom y y' in c

data Z = P | Q | R | S deriving (Eq, Show)

zIndex :: Z -> Int
zIndex P = 1
zIndex Q = 2
zIndex R = 3
zIndex S = 4

zEdges :: Matrix IntCost
zEdges =
  fromLists $
    (fmap . fmap)
      IntCost
      [ [Cost 0, Cost 2, Infinity, Infinity],
        [Infinity, Cost 0, Cost 2, Infinity],
        [Infinity, Infinity, Cost 0, Cost 1],
        [Cost 1, Infinity, Infinity, Cost 0]
      ]

instance VCategory Z IntCost where
  hom = distanceFunc zEdges zIndex zIndex

zDistance :: Z -> Z -> Cost Int
zDistance z z' = let (IntCost c) = hom z z' in c

phiEdges :: Matrix IntCost
phiEdges =
  fromLists $
    (fmap . fmap)
      IntCost
      [ [Infinity, Infinity, Infinity],
        [Cost 11, Infinity, Infinity],
        [Infinity, Infinity, Infinity],
        [Infinity, Cost 9, Infinity]
      ]

phi :: X -> Y -> Cost Int
phi x y =
  let ds = (distances xEdges) `quantMult` phiEdges `quantMult` (distances yEdges)
      (IntCost d) = getElem (xIndex x) (yIndex y) ds
   in d

psi :: Matrix IntCost
psi =
  fromLists $
    (fmap . fmap)
      IntCost
      [ [Infinity, Infinity, Infinity, Infinity],
        [Cost 11, Infinity, Cost 0, Infinity],
        [Cost 4, Infinity, Cost 4, Infinity]
      ]

phiPsi :: X -> Z -> Cost Int
phiPsi x z =
  let ds =
        (distances xEdges)
          `quantMult` phiEdges
          `quantMult` (distances yEdges)
          `quantMult` psi
          `quantMult` (distances zEdges)
      (IntCost d) = getElem (xIndex x) (zIndex z) ds
   in d
