module Graph.Path
  ( isPath,
    maxPath,
    minPath,
    costPath,
    pathWith,
  )
where

import Data.Maybe (isJust, mapMaybe)
import Graph.Arrow (Arrow (..))
import Monoid.Cost
import Safe.Foldable (maximumMay, minimumMay)
import Safe (headMay)

-- determine if there is at least one path between two vertices
isPath ::
  (Eq v, Arrow a v w, Monoid w) =>
  (v -> [a]) ->
  v ->
  v ->
  Bool
isPath arrowsFrom v1 v2 = isJust $ pathWith headMay arrowsFrom v1 v2

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
minPath = pathWith minimumMay

-- find the maximum weight path
maxPath ::
  (Eq v, Arrow a v w, Monoid w, Ord w) =>
  (v -> [a]) ->
  v ->
  v ->
  Maybe w
maxPath = pathWith maximumMay

-- find a path using a custom way to combine weights
pathWith ::
  (Eq v, Arrow a v w, Monoid w) =>
  ([w] -> Maybe w) ->
  (v -> [a]) ->
  v ->
  v ->
  Maybe w
pathWith = path2 []

-- minimum weight path, keeping track of visited vertices
path2 ::
  (Eq v, Arrow a v w, Monoid w) =>
  [v] ->
  ([w] -> Maybe w) ->
  (v -> [a]) ->
  v ->
  v ->
  Maybe w
path2 visited select arrowsFrom from to
  | from == to = Just mempty
  | from `elem` visited = Nothing
  | (not . null) paths = select paths
  | otherwise = Nothing
  where
    pathThrough a =
      fmap
        (weight a <>)
        (path2 (from : visited) select arrowsFrom (target a) to)
    paths = mapMaybe pathThrough (arrowsFrom from)
