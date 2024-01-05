module Ch2.Sec3.Figure18
  ( Vertex (..),
    Arrow (..),
    arrowsFrom,
    vertices,
  )
where

import Lib.Graph

data Vertex = A | B | C | D deriving (Eq, Show)

data Arrow = Arrow
  { from :: Vertex,
    to :: Vertex,
    weight :: Int
  }

instance Graph Vertex Arrow where
  source = from
  target = to

vertices :: [Vertex]
vertices = [A, B, C, D]

arrowsFrom :: Vertex -> [Arrow]
arrowsFrom A = [Arrow A C 3]
arrowsFrom B = [Arrow B A 2, Arrow B D 5]
arrowsFrom C = [Arrow C B 3]
arrowsFrom D = [Arrow D C 6]