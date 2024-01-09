module Ch2.Sec3.Exercise42
  ( City (..),
    Transport (..),
    Transports (..),
    Route (..),
    cities,
    transports,
    arrowsFrom,
  )
where

import Ch1.Set (isSubsetOf)
import Data.List (intersect)
import Graph.Graph
import Lib.Preorder as PO

data City = V | W | X | Y | Z deriving (Eq, Show)

cities :: [City]
cities = [V, W, X, Y, Z]

data Transport = A | B | C deriving (Eq, Show)

transports :: [Transport]
transports = [A, B, C]

data Route = Route
  { from :: City,
    to :: City,
    routeTransports :: Transports
  }

instance Arrow Route City Transports where
  source = from
  target = to
  weight = routeTransports

arrowsFrom :: City -> [Route]
arrowsFrom V = [Route V X (Transports [A, C])]
arrowsFrom W = [Route W Z (Transports [C, B])]
arrowsFrom X = [Route X Y (Transports [A])]
arrowsFrom Y = [Route Y Z (Transports [A, C])]
arrowsFrom Z =
  [ Route Z V (Transports [A, B]),
    Route Z X (Transports [B]),
    Route Z Y (Transports [C])
  ]

newtype Transports = Transports [Transport] deriving (Eq, Show)

instance Ord Transports where
  (Transports xs) <= (Transports ys) = xs `isSubsetOf` ys

instance Preorder Transports where
  (<=) = (Prelude.<=)

instance Monoid Transports where
  mempty = Transports transports

instance Semigroup Transports where
  (Transports xs) <> (Transports ys) = Transports (xs `intersect` ys)
