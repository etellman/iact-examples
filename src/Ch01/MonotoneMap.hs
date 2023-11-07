module Ch01.MonotoneMap
  ( arrow,
  )
where

import qualified Data.Set as S

-- from exercise 61
arrow :: Ord a => (a -> a -> Bool) -> a -> S.Set a -> S.Set a
arrow lte p = S.fromList . (filter (\p' -> p `lte` p')) . S.elems
