module Ch1.Graph
  ( Graph (..),
    compose,
    connections,
    path,
  )
where

class Graph v where
  vertices :: [v]
  arrows :: [(v, v)]

  source :: ((v, v) -> v)
  source (x, _) = x

  target :: ((v, v) -> v)
  target (_, x) = x

-- the target of two composed arrows, if possible
compose :: (Eq v, Graph v) => (v, v) -> (v, v) -> Maybe (v, v)
compose f g
  | target f == source g = Just $ (source f, target g)
  | otherwise = Nothing

-- direct connections between two vertices
connections :: (Eq v, Graph v) => v -> v -> [(v, v)]
connections v1 v2 = filter (\a -> source a == v1 && target a == v2) arrows

-- determine if there is at least one path between two vertices
path :: (Eq v, Graph v) => v -> v -> Bool
path = path' []

-- determine if there is at least one path between two vertices, keeping track of already visited
-- vertices
path' :: (Eq v, Graph v) => [v] -> v -> v -> Bool
path' visited from to =
  let children v = fmap target $ filter (\a -> source a == v) arrows
   in from == to
        || (not . null) (connections from to)
        || any
          (\v -> path' (from : visited) v to)
          (filter (\c -> not $ elem c visited) $ children from)
