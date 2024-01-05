module Ch2.Sec3.Example31
  ( Vertex (..),
    vertices,
    arrowsFrom,
  )
where

import Lib.Graph

data Vertex = P | Q | R | S | T deriving (Eq, Show)

newtype Arrow = Arrow (Vertex, Vertex)

instance Graph Vertex Arrow where
  source (Arrow (v, _)) = v
  target (Arrow (_, v)) = v

vertices :: [Vertex]
vertices = [P, Q, R, S, T]

arrowsFrom :: Vertex -> [Arrow]
arrowsFrom P = fmap Arrow [(P, Q), (P, R)]
arrowsFrom Q = fmap Arrow [(Q, S)]
arrowsFrom R = fmap Arrow [(R, S)]
arrowsFrom S = fmap Arrow [(S, T)]
arrowsFrom _ = []
