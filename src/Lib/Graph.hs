module Lib.Graph
  ( Arrow (..),
    isPath,
    maxPath,
    minPath,
    pathWith,
    IntWeight (..),
  )
where

import Data.Maybe (fromJust, isJust)
import Data.Monoid (Sum (..))

newtype IntWeight = IntWeight (Sum Int) deriving (Semigroup, Monoid, Eq, Ord, Show)

class Arrow a v w | a -> v w where
  source :: a -> v
  target :: a -> v
  weight :: a -> w

-- determine if there is at least one path between two vertices
isPath ::
  (Eq v, Arrow a v w, Monoid w, Ord w) =>
  (v -> [a]) ->
  v ->
  v ->
  Bool
isPath arrowsFrom v1 v2 = isJust $ minPath arrowsFrom v1 v2

-- find the minimum weight path
minPath ::
  (Eq v, Arrow a v w, Monoid w, Ord w) =>
  (v -> [a]) ->
  v ->
  v ->
  Maybe w
minPath arrowsFrom = pathWith arrowsFrom minimum

-- find the maximum weight path
maxPath ::
  (Eq v, Arrow a v w, Monoid w, Ord w) =>
  (v -> [a]) ->
  v ->
  v ->
  Maybe w
maxPath arrowsFrom = pathWith arrowsFrom maximum

-- find a path using a custom way to combine weights
pathWith ::
  (Eq v, Arrow a v w, Monoid w, Ord w) =>
  (v -> [a]) ->
  ([w] -> w) ->
  v ->
  v ->
  Maybe w
pathWith arrowsFrom select = path2 arrowsFrom select []

-- minimum weight path, keeping track of visited vertices
path2 ::
  (Eq v, Arrow a v w, Monoid w, Ord w) =>
  (v -> [a]) ->
  ([w] -> w) ->
  [v] ->
  v ->
  v ->
  Maybe w
path2 arrowsFrom select visited from to
  | from == to = Just $ mempty
  | from `elem` visited = Nothing
  | (not . null) paths = Just $ select paths
  | otherwise = Nothing
  where
    path2' = path2 arrowsFrom select
    pathThrough a =
      fmap
        (weight a <>)
        (path2' (from : visited) (target a) to)
    paths = fmap fromJust $ filter isJust $ fmap pathThrough (arrowsFrom from)
