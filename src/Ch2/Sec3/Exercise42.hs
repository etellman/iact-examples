module Ch2.Sec3.Exercise42
  ( City (..),
    Transport (..),
    Transports (..),
    Arrow (..),
    cities,
    transports,
    arrowsFrom,
  )
where

import Ch1.Set (isSubsetOf)
import Data.List (intersect)
import Lib.Graph
import Lib.Preorder as PO

data City = V | W | X | Y | Z deriving (Eq, Show)

cities :: [City]
cities = [V, W, X, Y, Z]

data Transport = A | B | C deriving (Eq, Show)

transports :: [Transport]
transports = [A, B, C]

data Arrow = Arrow
  { from :: City,
    to :: City,
    weight :: Transports
  }

instance Graph City Arrow where
  source = from
  target = to

arrowsFrom :: City -> [Arrow]
arrowsFrom V = [Arrow V X (Transports [A, C])]
arrowsFrom W = [Arrow W Z (Transports [C, B])]
arrowsFrom X = [Arrow X Y (Transports [A])]
arrowsFrom Y = [Arrow Y Z (Transports [A, C])]
arrowsFrom Z =
  [ Arrow Z V (Transports [A, B]),
    Arrow Z X (Transports [B]),
    Arrow Z Y (Transports [C])
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
