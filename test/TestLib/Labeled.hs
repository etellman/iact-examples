module TestLib.Labeled
  ( Labeled (..),
  )
where

-- use to generate types that aren't instances of Show with Hedgehog
data Labeled a = Labeled !String !a

instance Show (Labeled a) where
  show (Labeled l _) = l
