module Ch2.Sec3.Figure18
  ( Vertex (..),
    Fig18Arrow (..),
    arrowsFrom,
    vertices,
  )
where

import Graph.Arrow
import Graph.IntWeight

data Vertex = A | B | C | D deriving (Eq, Show)

data Fig18Arrow = Fig18Arrow
  { from :: !Vertex,
    to :: !Vertex,
    fig18weight :: !Int
  }

instance Arrow Fig18Arrow Vertex IntWeight where
  source = from
  target = to
  weight = toIntWeight . fig18weight

vertices :: [Vertex]
vertices = [A, B, C, D]

arrowsFrom :: Vertex -> [Fig18Arrow]
arrowsFrom A = [Fig18Arrow A C 3]
arrowsFrom B = [Fig18Arrow B A 2, Fig18Arrow B D 5]
arrowsFrom C = [Fig18Arrow C B 3]
arrowsFrom D = [Fig18Arrow D C 6]
