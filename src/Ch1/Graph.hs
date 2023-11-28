module Ch1.Graph
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
    source :: (a -> v),
    target :: (a -> v)
  }

instance (Show v) => Show (Graph v a) where
  show g = show (vertices g)

-- the target of two composed arrows, if possible
compose :: Eq v => Graph v a -> a -> a -> Maybe v
compose graph f g
  | target graph f == source graph g = Just $ target graph g
  | otherwise = Nothing

-- direct connections between two vertices
connections :: Eq v => Graph v a -> v -> v -> [a]
connections graph v1 v2 =
  let s = source graph
      t = target graph
   in filter (\a -> s a == v1 && t a == v2) (arrows graph)

-- determine if there is at least one path between two vertices
path :: Eq v => Graph v a -> v -> v -> Bool
path g = path' g []

-- determine if there is at least one path between two vertices, keeping track of already visited
-- vertices
path' :: Eq v => Graph v a -> [v] -> v -> v -> Bool
path' graph visited from to =
  let s = source graph
      t = target graph
      children v = fmap t $ filter (\a -> s a == v) (arrows graph)
   in from == to
        || (not . null) (connections graph from to)
        || any
          (\v -> path' graph (from : visited) v to)
          (filter (\c -> not $ elem c visited) $ children from)
