module Ch2.Sec4.Example55
  ( X (..),
    Y (..),
    xDistance,
    yDistance,
    xyDistance,
  )
where

import Data.Matrix
import Monoid.Cost
import Preorder.Quantale

data X = A | B | C deriving (Eq, Show)

xOrd :: X -> Int
xOrd A = 1
xOrd B = 2
xOrd C = 3

xDistance :: X -> X -> Cost Int
xDistance =
  let xCosts =
        fromLists
          [ [0, 2, Infinity],
            [Infinity, 0, 3],
            [Infinity, Infinity, Infinity]
          ]
   in distanceFunc xCosts xOrd xOrd

data Y = P | Q deriving (Eq, Show)

yOrd :: Y -> Int
yOrd P = 1
yOrd Q = 2

yDistance :: Y -> Y -> Cost Int
yDistance =
  let yCosts =
        fromLists
          [ [0, 5],
            [8, 0]
          ]
   in distanceFunc yCosts yOrd yOrd

xyDistance :: (X, Y) -> (X, Y) -> Cost Int
xyDistance (x, y) (x', y') = xDistance x x' <> yDistance y y'
