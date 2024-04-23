module Lib.VCategory (VCategory (..)) where

class VCategory m v where
  hom :: m -> m -> v
