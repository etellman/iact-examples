module Ch2.Sec3.Example31 (Ex31 (..)) where

import Data.Char (ord)
import Data.Matrix
import Preorder.Quantale

ex31Edges :: Matrix BoolWeight
ex31Edges =
  fromLists $
    (fmap . fmap)
      BoolWeight
      [ [True, True, True, False, False],
        [False, True, False, True, False],
        [False, False, True, True, False],
        [False, False, False, True, True],
        [False, False, False, False, True]
      ]

newtype Ex31 = Ex31 Char deriving (Eq)

instance Show Ex31 where
  show (Ex31 v) = show v

indexOf :: Char -> Int
indexOf v = ord v - ord 'p' + 1

ex31Distance :: Char -> Char -> BoolWeight
ex31Distance = distanceFunc ex31Edges indexOf indexOf

instance Ord Ex31 where
  Ex31 x <= Ex31 y =
    let (BoolWeight w) = ex31Distance x y
     in w
