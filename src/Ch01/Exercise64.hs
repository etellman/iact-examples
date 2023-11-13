module Ch01.Exercise64
  ( X (..),
    Y (..),
    P (..),
    xs,
    ys,
    fstar,
  )
where

data X = X Int deriving Show

data Y = Y Int deriving Show

data P = P Int deriving Show

xs :: [X]
xs = [X 1, X 2, X 3]

ys :: [Y]
ys = [Y 2, Y 4, Y 6]

fstar :: (X -> Y) -> (Y -> P) -> (X -> P)
fstar f s = s . f
