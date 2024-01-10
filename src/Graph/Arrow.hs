{-# LANGUAGE FunctionalDependencies #-}

module Graph.Arrow (Arrow (..)) where

class Arrow a v w | a -> v w where
  source :: a -> v
  target :: a -> v
  weight :: a -> w
