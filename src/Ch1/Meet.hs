module Ch1.Meet
  ( meet,
  join,
  )
where

import Ch1.Preorder

-- | finds the meet of a set within a preorder
meet :: Preorder a -> [a] -> a
meet (Preorder lte xs) xs' =
  let lessThanAll = filter (\x -> all (x `lte`) xs') xs
   in foldr1 (\x a -> if x `lte` a then a else x) lessThanAll

-- | finds the join of a set within a preorder
join :: Preorder a -> [a] -> a
join (Preorder lte xs) = meet (Preorder (flip lte) xs)
