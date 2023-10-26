module Ch01.Joinable (Joinable (..)) where

class Joinable a where
  join :: a -> a -> a
