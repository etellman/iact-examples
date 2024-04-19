module Lib.VCategory (VCategory (..)) where

class (Monoid v) => VCategory m v where
  hom :: m -> m -> v
