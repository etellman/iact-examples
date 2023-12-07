module Ch1.Sec4.Example109
  ( A (..),
    B (..),
    fn,
    fstar,
    anyA,
    allAs,
  )
where

import Control.Monad (guard)

newtype A = A Int deriving (Show, Eq, Ord)

newtype B = B Int deriving (Show, Eq, Ord)

fn :: Int -> A -> B
fn n (A a) = B $ a `mod` n

fstar :: (A -> B) -> [A] -> [B] -> [A]
fstar f as bs = do
  a <- as
  b <- bs
  guard $ f a == b
  return a

anyA :: (A -> B) -> [B] -> [A] -> [B]
anyA f bs as' = do
  b <- bs
  guard $ any (\a -> f a == b) as'
  return b

allAs :: (A -> B) -> [A] -> [B] -> [A] -> [B]
allAs f as bs as' = do
  b <- bs
  guard $ all (\a -> f a /= b || a `elem` as') as
  return b
