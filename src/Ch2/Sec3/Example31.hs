module Ch2.Sec3.Example31
  ( Ex31 (..),
    ex31ToBool,
  )
where

import Ch2.Sec2.BooleanMonoids (BooleanAnd (..))
import Lib.Preorder

data Ex31 = P | Q | R | S | T deriving (Eq, Show)

instance Preorder Ex31 where
  P `lte` Q = True
  P `lte` R = True
  P `lte` S = True
  P `lte` T = True
  Q `lte` S = True
  Q `lte` T = True
  R `lte` S = True
  R `lte` T = True
  S `lte` T = True
  x `lte` y = x == y

ex31ToBool :: Ex31 -> Ex31 -> BooleanAnd
ex31ToBool x y = BooleanAnd $ x `lte` y
