module Ch2.Sec3.Example31
  ( Ex31 (..),
    ex31ToBool,
    vertices,
    arrowsFrom,
  )
where

import Ch2.Sec2.BooleanMonoids (BooleanAnd (..))
import Lib.Graph
import Lib.Preorder as PO

data Ex31 = P | Q | R | S | T deriving (Eq, Show)

newtype Ex31Arrow = Ex31Arrow (Ex31, Ex31)

instance Graph Ex31 Ex31Arrow where
  vertices = [P, Q, R, S, T]

  source (Ex31Arrow (v, _)) = v
  target (Ex31Arrow (_, v)) = v

arrowsFrom :: Ex31 -> [Ex31Arrow]
arrowsFrom P = fmap Ex31Arrow [(P, Q), (P, R)]
arrowsFrom Q = fmap Ex31Arrow [(Q, S)]
arrowsFrom R = fmap Ex31Arrow [(R, S)]
arrowsFrom S = fmap Ex31Arrow [(S, T)]
arrowsFrom _ = []

instance Preorder Ex31 where
  (<=) = isPath arrowsFrom

ex31ToBool :: Ex31 -> Ex31 -> BooleanAnd
ex31ToBool x y = BooleanAnd $ x PO.<= y
