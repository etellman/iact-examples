module Ch2.Sec3.Example31
  ( Vertex (..),
    vertices,
    arrowsFrom,
  )
where

import Data.Monoid (Sum (..))
import Lib.Graph

data Vertex = P | Q | R | S | T deriving (Eq, Show)

newtype Ex31Arrow = Ex31Arrow (Vertex, Vertex)

instance Arrow Ex31Arrow Vertex IntWeight where
  source (Ex31Arrow (v, _)) = v
  target (Ex31Arrow (_, v)) = v
  weight' = const $ IntWeight (Sum 1)

vertices :: [Vertex]
vertices = [P, Q, R, S, T]

arrowsFrom :: Vertex -> [Ex31Arrow]
arrowsFrom P = fmap Ex31Arrow [(P, Q), (P, R)]
arrowsFrom Q = fmap Ex31Arrow [(Q, S)]
arrowsFrom R = fmap Ex31Arrow [(R, S)]
arrowsFrom S = fmap Ex31Arrow [(S, T)]
arrowsFrom _ = []
