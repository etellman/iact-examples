{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Ch3.Sec4.AdjunctionExample
  ( A (..),
    B (..),
    getB,
  )
where

import Data.Distributive
import Data.Functor.Rep
import GHC.Generics (Generic1)

newtype A a = A a deriving (Show, Eq)

instance Functor A where
  fmap f (A x) = A (f x)

newtype B b = B b deriving (Show, Eq, Generic1)

getB :: B b -> b
getB (B x) = x

instance Functor B where
  fmap f (B x) = B (f x)

instance Distributive B where
  distribute :: Functor f => f (B a) -> B (f a)
  distribute = B . fmap getB

  collect :: Functor f => (a -> B b) -> f a -> B (f b)
  collect f = B . fmap (getB . f)

instance Representable B
