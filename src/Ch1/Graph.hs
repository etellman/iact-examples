module Ch1.Graph
  ( Graph (..),
    connections,
    path,
  )
where

class Graph v a | v -> a where
  vertices :: [v]
  arrowsFrom :: v -> [a]
  source :: a -> v
  target :: a -> v
  weight :: a -> Int

-- direct connections between two vertices
connections :: (Eq v, Graph v a) => v -> v -> [a]
connections v1 v2 =
  let targetMatches a = target a == v2
    in filter targetMatches (arrowsFrom v1)

-- determine if there is at least one path between two vertices
path :: (Eq v, Graph v a) => v -> v -> Bool
path = path' []

-- determine if there is at least one path between two vertices, keeping track of already visited
-- vertices
path' :: (Eq v, Graph v a) => [v] -> v -> v -> Bool
path' visited from to =
  let children = (fmap target) . arrowsFrom $ from
      unvisited v = not $ elem v visited
   in from == to
        || (not . null) (connections from to)
        || any
          (\v -> path' (from : visited) v to)
          (filter unvisited children)
