module Ch1.Graph
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

-- direct connections between two vertices
connections :: (Eq v, Graph v a) => v -> v -> [a]
connections v1 v2 =
  let targetMatches a = target a == v2
   in filter targetMatches (arrowsFrom v1)

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
  | (not . null) direct = Just $ minimum direct
  | (not . null) indirect = minimum indirect
  | otherwise = Nothing
  where
    unvisited a = not $ elem (target a) visited
    direct = fmap weight $ connections from to
    pathThrough a =
      fmap
        (weight a <>)
        (path2 weight (target a : visited) (target a) to)
    indirect = fmap pathThrough (filter unvisited (arrowsFrom from))
