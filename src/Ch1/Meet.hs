module Ch1.Meet
  ( meet,
    join,
  )
where

import Preorder.Preorder as PO

meet' :: (a -> a -> Bool) -> [a] -> [a] -> a
meet' f xs xs' =
  let lessThanAll = filter (\x -> all (f x) xs') xs
   in foldr1 (\x a -> if f a x then x else a) lessThanAll

-- | finds the meet of a set within a preorder
meet ::
  Preorder a =>
  -- | all the elements in the preorder
  [a] ->
  -- | the subset for the meet
  [a] ->
  -- | the meet
  a
meet = meet' (PO.<=)

-- | finds the join of a set within a preorder
join ::
  Preorder a =>
  -- | all the elements in the preorder
  [a] ->
  -- | the subset for the join
  [a] ->
  -- | the join
  a
join = meet' (\x y -> y PO.<= x)
