module Ch3.Sec3.Exercise53
  ( verticesG,
    VertexG (..),
    arrowsG,
    verticesH,
    arrowsH,
    alphaVertex,
    alphaArrow,
  )
where

import Graph.Arrow
import Graph.IntWeight

data VertexG = V1 | V2 | V3 deriving (Eq, Show)

data ArrowG = ArrowG !String !(VertexG, VertexG) deriving (Eq, Show)

instance Arrow ArrowG VertexG IntWeight where
  source (ArrowG _ (v, _)) = v
  target (ArrowG _ (_, v)) = v
  weight = const unitWeight

verticesG :: [VertexG]
verticesG = [V1, V2, V3]

arrowsG :: [ArrowG]
arrowsG = [ArrowG "a" (V1, V2), ArrowG "b" (V2, V3)]

data VertexH = V4 | V5 deriving (Eq, Show)

data ArrowH = ArrowH !String !(VertexH, VertexH) deriving (Eq, Show)

instance Arrow ArrowH VertexH IntWeight where
  source (ArrowH _ (v, _)) = v
  target (ArrowH _ (_, v)) = v
  weight = const unitWeight

verticesH :: [VertexH]
verticesH = [V4, V5]

arrowsH :: [ArrowH]
arrowsH = [ArrowH "c" (V4, V5), ArrowH "d" (V4, V5), ArrowH "e" (V5, V5)]

alphaVertex :: VertexG -> VertexH
alphaVertex V1 = V4
alphaVertex V2 = V5
alphaVertex V3 = V5

alphaArrow :: ArrowG -> ArrowH
alphaArrow (ArrowG "a" (V1, V2)) = ArrowH "d" (V4, V5)
alphaArrow (ArrowG "b" (V2, V3)) = ArrowH "e" (V5, V5)
alphaArrow _ = error "unexpected arrow"
