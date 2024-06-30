module Ch4.Sec2.Exercise2
  ( X (..),
    Y (..),
    XY (..),
    phi,
  )
where

import Data.PartialOrd as PO

data X = Monoid | Category | Preorder deriving (Eq, Show)

data Y = NoBook | Book deriving (Eq, Show)

instance PartialOrd X where
  Monoid <= Category = True
  Preorder <= Category = True
  x <= y = x Prelude.== y

instance PartialOrd Y where
  NoBook <= Book = True
  x <= y = x Prelude.== y

data XY = XY !X !Y deriving (Eq, Show)

instance PartialOrd XY where
  XY x1 y1 <= XY x2 y2 = x2 PO.<= x1 && y1 PO.<= y2

-- The aunt needs to read book in order to explain any of this
phi :: XY -> Bool
phi (XY _ Book) = True
phi _ = False
