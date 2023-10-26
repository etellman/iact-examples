module Ch01.BooleanSystem
  ( BooleanSystem (..),
  )
where

import Ch01.Joinable

newtype BooleanSystem = BooleanSystem Bool deriving (Eq, Show)

instance Joinable BooleanSystem where
  join (BooleanSystem x) (BooleanSystem y) = BooleanSystem $ x || y

instance Ord BooleanSystem where
  (BooleanSystem x) <= (BooleanSystem y) = not x || y
