module Ch01.Graph
  ( Graph (..),
    compose,
  )
where

data Graph v a = Graph [v] [a] (a -> v) (a -> v)

compose :: Eq v => Graph v a -> a -> a -> v
compose (Graph _ _ s t) f g
  | (t f) == (s g) = t g
  | otherwise = undefined
