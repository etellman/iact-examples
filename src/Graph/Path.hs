module Graph.Path
  ( isPath,
    maxPath,
    minPath,
    costPath,
    pathWith,
  )
where

import Data.Maybe (fromJust, isJust)
import Graph.Arrow (Arrow (..))
import Monoid.Cost

-- determine if there is at least one path between two vertices
isPath ::
  (Eq v, Arrow a v w, Monoid w) =>
  (v -> [a]) ->
  v ->
  v ->
  Bool
isPath arrowsFrom v1 v2 = isJust $ pathWith head arrowsFrom v1 v2

-- find the minimum cost path
costPath ::
  (Eq v, Arrow a v w, Monoid w, Ord w) =>
  (w -> Cost) ->
  (v -> [a]) ->
  v ->
  v ->
  Cost
costPath toCost arrowsFrom v1 v2 =
  maybe Infinity toCost (minPath arrowsFrom v1 v2)

-- find the minimum weight path
minPath ::
  (Eq v, Arrow a v w, Monoid w, Ord w) =>
  (v -> [a]) ->
  v ->
  v ->
  Maybe w
minPath = pathWith minimum

-- find the maximum weight path
maxPath ::
  (Eq v, Arrow a v w, Monoid w, Ord w) =>
  (v -> [a]) ->
  v ->
  v ->
  Maybe w
maxPath = pathWith maximum

-- find a path using a custom way to combine weights
pathWith ::
  (Eq v, Arrow a v w, Monoid w) =>
  ([w] -> w) ->
  (v -> [a]) ->
  v ->
  v ->
  Maybe w
pathWith = path2 []

-- minimum weight path, keeping track of visited vertices
path2 ::
  (Eq v, Arrow a v w, Monoid w) =>
  [v] ->
  ([w] -> w) ->
  (v -> [a]) ->
  v ->
  v ->
  Maybe w
path2 visited select arrowsFrom from to
  | from == to = Just mempty
  | from `elem` visited = Nothing
  | (not . null) paths = Just $ select paths
  | otherwise = Nothing
  where
    pathThrough a =
      fmap
        (weight a <>)
        (path2 (from : visited) select arrowsFrom (target a) to)
    paths = fmap fromJust $ filter isJust $ fmap pathThrough (arrowsFrom from)
