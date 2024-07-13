module Ch2.Sec3.Exercise42
  ( City (..),
    Transport (..),
    TransportSet (..),
  )
where

import Data.List (intersect)

data City = V | W | X | Y | Z deriving (Eq, Show)

data Transport = A | B | C deriving (Eq, Show)

data TransportSet = TransportSet [Transport] deriving (Eq)

instance Show TransportSet where
  show (TransportSet xs) = show xs

instance Semigroup TransportSet where
  (TransportSet xs) <> (TransportSet ys) = TransportSet (xs `intersect` ys)

instance Ord TransportSet where
  (TransportSet xs) <= (TransportSet ys) = length xs Prelude.>= length ys
