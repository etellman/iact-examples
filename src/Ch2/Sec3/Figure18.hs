module Ch2.Sec3.Figure18
  ( xWeights,
    distanceX,
  )
where

import Data.Char (ord)
import Data.Matrix
import Monoid.Cost
import Preorder.Quantale

xWeights :: Matrix (Cost Int)
xWeights =
  fromLists
    [ [0, Infinity, 3, Infinity],
      [2, 0, Infinity, 5],
      [Infinity, 3, 0, Infinity],
      [Infinity, Infinity, 6, 0]
    ]

indexOf :: Char -> Int
indexOf v = ord v - ord 'A' + 1

distanceX :: Char -> Char -> Cost Int
distanceX = distanceFunc xWeights indexOf indexOf
