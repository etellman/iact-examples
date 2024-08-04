module Ch4.Sec4.Exercise39
  ( fC,
    fD,
    gE,
    gF,
    h,
  )
where

fC :: Int -> Int
fC = abs

fD :: Int -> Int
fD = (* 5)

gE :: Int -> Int -> Bool
gE = (<=)

gF :: Int -> Int -> Int
gF = (-)

h :: Int -> Bool -> Int
h c True = c
h c False = 1 - c
