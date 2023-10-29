module Ch01.Graph
  ( Graph (..),
    compose,
    connections,
    path,
  )
where

-- import Data.List (elem)

data Graph v a = Graph
  { vertices :: [v],
    arrows :: [a],
    s :: (a -> v),
    t :: (a -> v)
  }

-- the target of two composed arrows, if possible
compose :: Eq v => Graph v a -> a -> a -> Maybe v
compose graph f g
  | t graph f == s graph g = Just $ t graph g
  | otherwise = Nothing

-- direct connections between two vertices
connections :: Eq v => Graph v a -> v -> v -> [a]
connections graph v1 v2 = filter (\a -> s graph a == v1 && t graph a == v2) (arrows graph)

-- determine if there is at least one path between two vertices
path :: Eq v => Graph v a -> v -> v -> Bool
path g = path' g []

-- determine if there is at least one path between two vertices, keeping track of already visited
-- vertices
path' :: Eq v => Graph v a -> [v] -> v -> v -> Bool
path' graph visited from to =
  let children v = fmap (t graph) $ filter (\a -> s graph a == v) (arrows graph)
   in from == to
        || (not . null) (connections graph from to)
        || any
          (\v -> path' graph (from : visited) v to)
          (filter (\c -> not $ elem c visited) $ children from)
