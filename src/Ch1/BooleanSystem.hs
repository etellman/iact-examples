module Ch1.BooleanSystem
  ( BooleanSystem (..),
  )
where

import Ch1.Joinable
import Data.PartialOrd

newtype BooleanSystem = BooleanSystem Bool deriving (Eq, Show)

instance Joinable BooleanSystem where
  join (BooleanSystem x) (BooleanSystem y) = BooleanSystem $ x || y

instance PartialOrd BooleanSystem where
  (BooleanSystem x) <= (BooleanSystem y) = not x || y
