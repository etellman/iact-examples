module Ch1.Graph
  ( Graph (..),
    isPath,
    shortestPath,
  )
where

import Data.Maybe (isJust)

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
isPath v1 v2 = isJust $ shortestPath (const 1) v1 v2

-- find the minimum weight path
shortestPath :: (Eq v, Graph v a) => (a -> Int) -> v -> v -> Maybe Int
shortestPath weight = path2 weight []

-- minimum weight path, keeping track of visited vertices
path2 :: (Eq v, Graph v a) => (a -> Int) -> [v] -> v -> v -> Maybe Int
path2 weight visited from to
  | from == to = Just 0
  | (not . null) direct = Just $ minimum direct
  | (not . null) indirect = minimum indirect
  | otherwise = Nothing
  where
    unvisited a = not $ elem (target a) visited
    direct = fmap weight $ connections from to
    unvisitedArrows = (filter unvisited (arrowsFrom from))
    pathThrough a = path2 ((+ weight a) . weight) (from : visited) (target a) to
    indirect = fmap pathThrough unvisitedArrows
