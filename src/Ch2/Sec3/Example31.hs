module Ch2.Sec3.Example31
  ( Ex31 (..),
    ex31ToBool,
  )
where

import Ch1.Graph
import Ch2.Sec2.BooleanMonoids (BooleanAnd (..))
import Lib.Preorder as PO

data Ex31 = P | Q | R | S | T deriving (Eq, Show)

instance Graph Ex31 where
  vertices = [P, Q, R, S, T]
  arrows =
    [ (P, Q),
      (P, R),
      (Q, S),
      (R, S),
      (S, T)
    ]

instance Preorder Ex31 where
  (<=) = path

ex31ToBool :: Ex31 -> Ex31 -> BooleanAnd
ex31ToBool x y = BooleanAnd $ x PO.<= y
