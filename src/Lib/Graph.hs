module Lib.Graph
  ( Graph (..),
    isPath,
    maxPath,
    minPath,
    shortestPath,
    pathWith,
  )
where

import Data.Maybe (fromJust, isJust)
import Data.Monoid (Sum (..))

class Graph v a w | a -> v w where
  source :: a -> v
  target :: a -> v
  weight' :: a -> w

-- determine if there is at least one path between two vertices
isPath ::
  (Eq v, Graph v a w) =>
  (v -> [a]) ->
  v ->
  v ->
  Bool
isPath arrowsFrom v1 v2 = isJust $ shortestPath arrowsFrom v1 v2

-- find the path with the fewest edges
shortestPath ::
  (Eq v, Graph v a w) =>
  (v -> [a]) ->
  v ->
  v ->
  Maybe Int
shortestPath arrowsFrom v1 v2 = fmap getSum $ minPath arrowsFrom (const $ Sum 1) v1 v2

-- find the minimum weight path
minPath ::
  (Eq v, Graph v a w, Monoid m, Ord m) =>
  (v -> [a]) ->
  (a -> m) ->
  v ->
  v ->
  Maybe m
minPath arrowsFrom = pathWith arrowsFrom minimum

-- find the maximum weight path
maxPath ::
  (Eq v, Graph v a w, Monoid m, Ord m) =>
  (v -> [a]) ->
  (a -> m) ->
  v ->
  v ->
  Maybe m
maxPath arrowsFrom = pathWith arrowsFrom maximum

-- find a path using a custom way to combine weights
pathWith ::
  (Eq v, Graph v a w, Monoid m, Ord m) =>
  (v -> [a]) ->
  ([m] -> m) ->
  (a -> m) ->
  v ->
  v ->
  Maybe m
pathWith arrowsFrom select weight = path2 arrowsFrom select weight []

-- minimum weight path, keeping track of visited vertices
path2 ::
  (Eq v, Graph v a w, Monoid m, Ord m) =>
  (v -> [a]) ->
  ([m] -> m) ->
  (a -> m) ->
  [v] ->
  v ->
  v ->
  Maybe m
path2 arrowsFrom select weight visited from to
  | from == to = Just $ mempty
  | from `elem` visited = Nothing
  | (not . null) paths = Just $ select paths
  | otherwise = Nothing
  where
    path2' = path2 arrowsFrom select weight
    pathThrough a =
      fmap
        (weight a <>)
        (path2' (from : visited) (target a) to)
    paths = fmap fromJust $ filter isJust $ fmap pathThrough (arrowsFrom from)
