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

xDistance :: X -> X -> IntCost
xDistance =
  let costs =
        fromLists $
          (fmap . fmap)
            IntCost
            [ [Cost 0, Infinity, Cost 3, Infinity],
              [Cost 2, Cost 0, Infinity, Cost 5],
              [Infinity, Cost 3, Cost 0, Infinity],
              [Infinity, Infinity, Cost 4, Cost 0]
            ]
   in distanceFunc costs xOrd xOrd

instance VCategory X IntCost where
  hom = xDistance

data Y = X | Y | Z deriving (Show, Eq)

yOrd :: Y -> Int
yOrd X = 1
yOrd Y = 2
yOrd Z = 3

yDistance :: Y -> Y -> IntCost
yDistance =
  let costs =
        fromLists $
          (fmap . fmap)
            IntCost
            [ [Cost 0, Cost 4, Cost 3],
              [Cost 3, Cost 0, Infinity],
              [Infinity, Cost 3, Cost 0]
            ]
   in distanceFunc costs yOrd yOrd

instance VCategory Y IntCost where
  hom = yDistance

data XY = XY !X !Y deriving (Show, Eq)

instance VCategory XY IntCost where
  hom (XY x y) (XY x' y') = hom x' x <> hom y y'

bridge :: X -> Y -> IntCost
bridge B X = IntCost $ Cost 11
bridge D Y = IntCost $ Cost 9
bridge _ _ = IntCost $ Infinity

-- | what is the cost to go from x to y via x' and y'
cost' :: X -> Y -> X -> Y -> Cost Int
cost' x y x' y' =
  let (IntCost c) = hom x x' <> bridge x' y' <> hom y' y
   in c

-- | what is the minimum cost to go from to y?
cost :: X -> Y -> Cost Int
cost x y =
  let c = cost' x y
   in min (c B X) (c D Y)
