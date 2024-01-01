module Lib.Graph
  ( Graph (..),
    isPath,
    minPath,
    shortestPath,
  )
where

import Data.Maybe (isJust)
import Data.Monoid (Sum (..))

class Graph v a | v -> a where
  vertices :: [v]
  arrowsFrom :: v -> [a]
  source :: a -> v
  target :: a -> v

-- determine if there is at least one path between two vertices
isPath :: (Eq v, Graph v a) => v -> v -> Bool
isPath v1 v2 = isJust $ shortestPath v1 v2

-- find the path with the fewest edges
shortestPath :: (Eq v, Graph v a) => v -> v -> Maybe Int
shortestPath v1 v2 = fmap getSum $ minPath (const $ Sum 1) v1 v2

-- find the minimum weight path
minPath :: (Eq v, Graph v a, Monoid m, Ord m) => (a -> m) -> v -> v -> Maybe m
minPath weight = path2 weight []

-- minimum weight path, keeping track of visited vertices
path2 :: (Eq v, Graph v a, Monoid m, Ord m) => (a -> m) -> [v] -> v -> v -> Maybe m
path2 weight visited from to
  | from == to = Just $ mempty
  | from `elem` visited = Nothing
  | (not . null) paths = minimum paths
  | otherwise = Nothing
  where
    pathThrough a =
      fmap
        (weight a <>)
        (path2 weight (from : visited) (target a) to)
    paths = filter isJust $ fmap pathThrough (arrowsFrom from)
