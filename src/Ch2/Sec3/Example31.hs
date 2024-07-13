module Ch2.Sec3.Example31
  ( Vertex (..),
    vertices,
    arrowsFrom,
    ex31,
    lte31,
  )
where

import Data.Char (ord)
import Data.Matrix
import Graph.Arrow
import Graph.IntWeight
import Preorder.Quantale

data Vertex = P | Q | R | S | T deriving (Eq, Show)

newtype Ex31Arrow = Ex31Arrow (Vertex, Vertex)

instance Arrow Ex31Arrow Vertex IntWeight where
  source (Ex31Arrow (v, _)) = v
  target (Ex31Arrow (_, v)) = v
  weight = const unitWeight

vertices :: [Vertex]
vertices = [P, Q, R, S, T]

arrowsFrom :: Vertex -> [Ex31Arrow]
arrowsFrom P = fmap Ex31Arrow [(P, Q), (P, R)]
arrowsFrom Q = fmap Ex31Arrow [(Q, S)]
arrowsFrom R = fmap Ex31Arrow [(R, S)]
arrowsFrom S = fmap Ex31Arrow [(S, T)]
arrowsFrom _ = []

ex31 :: Matrix BoolWeight
ex31 =
  fromLists $
    (fmap . fmap)
      BoolWeight
      [ [True, True, True, False, False],
        [False, True, False, True, False],
        [False, False, True, True, False],
        [False, False, False, True, True],
        [False, False, False, False, True]
      ]

lte31 :: Char -> Char -> Bool
lte31 x y =
  let f = distanceFunc ex31 (\v -> ord v - ord 'p' + 1)
      BoolWeight w = f x y
   in w
