module Ch2.Sec3.Example31
  ( ex31,
    lte31,
    Ex31 (..),
  )
where

import Data.Char (ord)
import Data.Matrix
import Preorder.Quantale

ex31 :: Matrix BoolWeight
ex31 =
  fromLists $
    (fmap . fmap)
      BoolWeight
      [ [True, True, True, False, False],
        [False, True, False, True, False],
        [False, False, True, True, False],
        [False, False, False, True, True],
        [False, False, False, False, True]
      ]

lte31 :: Char -> Char -> Bool
lte31 x y =
  let f = distanceFunc ex31 (\v -> ord v - ord 'p' + 1)
      BoolWeight w = f x y
   in w

newtype Ex31 = Ex31 Char deriving (Eq)

instance Show Ex31 where
  show (Ex31 v) = show v

instance Ord Ex31 where
  Ex31 x <= Ex31 y = x `lte31` y
