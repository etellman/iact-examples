module Ch4.Sec2.Example32
  ( X (..),
    xDistance,
    Y (..),
    yDistance,
    phi,
    col,
  )
where

import Data.Matrix
import Lib.VCategory (VCategory (..))
import Monoid.Cost
import Preorder.Quantale

data X = A | B deriving (Eq, Show)

xIndex :: X -> Int
xIndex A = 1
xIndex B = 2

xEdges :: Matrix IntCost
xEdges =
  fromLists $
    (fmap . fmap)
      IntCost
      [ [Cost 0, Cost 2],
        [Infinity, Cost 0]
      ]

instance VCategory X IntCost where
  hom = distanceFunc xEdges xIndex xIndex

xDistance :: X -> X -> Cost Int
xDistance x x' = let (IntCost c) = hom x x' in c

data Y = X | Y deriving (Eq, Show)

yIndex :: Y -> Int
yIndex X = 1
yIndex Y = 2

yEdges :: Matrix IntCost
yEdges =
  fromLists $
    (fmap . fmap)
      IntCost
      [ [Cost 0, Cost 3],
        [Cost 4, Cost 0]
      ]

instance VCategory Y IntCost where
  hom = distanceFunc yEdges yIndex yIndex

yDistance :: Y -> Y -> Cost Int
yDistance y y' = let (IntCost c) = hom y y' in c

phiEdges :: Matrix IntCost
phiEdges =
  fromLists $
    (fmap . fmap)
      IntCost
      [ [Cost 5, Infinity],
        [Infinity, Infinity]
      ]

phi :: X -> Y -> Cost Int
phi x y =
  let ds = (distances xEdges) `quantMult` phiEdges `quantMult` (distances yEdges)
      (IntCost d) = getElem (xIndex x) (yIndex y) ds
   in d

col :: Either X Y -> Either X Y -> Cost Int
col (Left x) (Left x') = xDistance x x'
col (Right y) (Right y') = yDistance y y'
col (Right _) (Left _) = Infinity
col (Left x) (Right y) = phi x y
