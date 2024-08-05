module Ch4.Sec4.Exercise39 (f, g, h, q) where

f :: Int -> (Int, Int)
f a = (abs a, a * 5)

g :: (Int, Int) -> (Bool, Int)
g (b, d) = (d <= b, d - b)

h :: (Int, Bool) -> Int
h (c, True) = c
h (c, False) = 1 - c

q :: (Int, Int) -> (Int, Int)
q (a, b) =
  let (c, d) = f a
      (e, f') = g (b, d)
      g' = h (c, e)
   in (f', g')
