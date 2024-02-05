module Ch1.Meet
  ( meet,
    join,
  )
where

import Preorder.Preorder as PO

meet' :: (a -> a -> Bool) -> [a] -> [a] -> Maybe a
meet' f xs as =
  let lessThanAll = filter (\x -> all (f x) as) xs
   in case lessThanAll of
        [] -> Nothing
        (y : ys) -> Just $ foldr (\x a -> if f a x then x else a) y ys

-- | finds the meet of a set within a preorder
meet ::
  Preorder a =>
  -- | all the elements in the preorder
  [a] ->
  -- | the subset for the meet
  [a] ->
  -- | the meet
  Maybe a
meet = meet' (PO.<=)

-- | finds the join of a set within a preorder
join ::
  Preorder a =>
  -- | all the elements in the preorder
  [a] ->
  -- | the subset for the join
  [a] ->
  -- | the join
  Maybe a
join = meet' $ flip (PO.<=)
