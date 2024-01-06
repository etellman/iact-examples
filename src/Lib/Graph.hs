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
  (Eq v, Arrow a v w, Monoid w) =>
  (v -> [a]) ->
  v ->
  v ->
  Bool
isPath arrowsFrom v1 v2 = isJust $ pathWith head arrowsFrom v1 v2

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
pathWith select arrowsFrom = path2 select arrowsFrom []

-- minimum weight path, keeping track of visited vertices
path2 ::
  (Eq v, Arrow a v w, Monoid w) =>
  ([w] -> w) ->
  (v -> [a]) ->
  [v] ->
  v ->
  v ->
  Maybe w
path2 select arrowsFrom visited from to
  | from == to = Just $ mempty
  | from `elem` visited = Nothing
  | (not . null) paths = Just $ select paths
  | otherwise = Nothing
  where
    pathThrough a =
      fmap
        (weight a <>)
        (path2 select arrowsFrom (from : visited) (target a) to)
    paths = fmap fromJust $ filter isJust $ fmap pathThrough (arrowsFrom from)
