module Ch2.Sec3.Figure18
  ( Vertex (..),
    Arrow (..),
    arrowsFrom,
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
  vertices = [A, B, C, D]

  source = from
  target = to

arrowsFrom :: Vertex -> [Arrow]
arrowsFrom A = [Arrow A C 3]
arrowsFrom B = [Arrow B A 2, Arrow B D 5]
arrowsFrom C = [Arrow C B 3]
arrowsFrom D = [Arrow D C 6]
