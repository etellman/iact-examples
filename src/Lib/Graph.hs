module Lib.Graph
  ( Graph (..),
    isPath,
    maxPath,
    minPath,
    shortestPath,
    pathWith,
  )
where

import Data.Maybe (isJust, fromJust)
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
minPath ::
  (Eq v, Graph v a, Monoid m, Ord m) =>
  (a -> m) ->
  v ->
  v ->
  Maybe m
minPath = pathWith minimum

-- find the maximum weight path
maxPath ::
  (Eq v, Graph v a, Monoid m, Ord m) =>
  (a -> m) ->
  v ->
  v ->
  Maybe m
maxPath = pathWith maximum

-- find a path using a custom way to combine weights
pathWith ::
  (Eq v, Graph v a, Monoid m, Ord m) =>
  ([m] -> m) ->
  (a -> m) ->
  v ->
  v ->
  Maybe m
pathWith select weight = path2 select weight []

-- minimum weight path, keeping track of visited vertices
path2 ::
  (Eq v, Graph v a, Monoid m, Ord m) =>
  ([m] -> m) ->
  (a -> m) ->
  [v] ->
  v ->
  v ->
  Maybe m
path2 select weight visited from to
  | from == to = Just $ mempty
  | from `elem` visited = Nothing
  | (not . null) paths = Just $ select paths
  | otherwise = Nothing
  where
    path2' = path2 select weight
    pathThrough a =
      fmap
        (weight a <>)
        (path2' (from : visited) (target a) to)
    paths = fmap fromJust $ filter isJust $ fmap pathThrough (arrowsFrom from)
