module Ch01.MonotoneMap
  ( arrow,
  )
where

-- from exercise 61
arrow :: (a -> a -> Bool) -> a -> [a] -> [a]
arrow lte p = filter (\p' -> p `lte` p')
