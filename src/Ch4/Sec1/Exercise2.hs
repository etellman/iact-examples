module Ch4.Sec1.Exercise2
  ( X (..),
    Y (..),
    XY (..),
    phi,
  )
where

import Preorder.Preorder as PO

data X = Monoid | Category | Preorder deriving (Eq, Show)

data Y = NoBook | Book deriving (Eq, Show)

instance PO.Preorder X where
  Monoid <= Category = True
  Preorder <= Category = True
  x <= y = x == y

instance PO.Preorder Y where
  NoBook <= Book = True
  x <= y = x == y

data XY = XY !X !Y deriving (Eq, Show)

instance PO.Preorder XY where
  XY x1 y1 <= XY x2 y2 = x2 PO.<= x1 && y1 PO.<= y2

phi :: XY -> Bool
phi (XY _ Book) = True
phi _ = False
