{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Ch3.Sec4.AdjunctionExample
  ( C (..),
    D (..),
    getD,
  )
where

import Data.Distributive
import Data.Functor.Adjunction
import Data.Functor.Rep
import GHC.Generics (Generic1)

newtype C a = C a deriving (Show, Eq)

instance Functor C where
  fmap f (C x) = C (f x)

newtype D b = D b deriving (Show, Eq, Generic1)

getD :: D b -> b
getD (D x) = x

instance Functor D where
  fmap f (D x) = D (f x)

instance Distributive D where
  distribute :: Functor f => f (D a) -> D (f a)
  distribute = D . fmap getD

  collect :: Functor f => (a -> D b) -> f a -> D (f b)
  collect f = D . fmap (getD . f)

instance Representable D

instance Adjunction C D where
  unit :: a -> D (C a)
  unit a = D (C a)

  counit :: C (D a) -> a
  counit (C (D a)) = a

  leftAdjunct :: (C a -> b) -> a -> D b
  leftAdjunct f a = D (f $ C a)

  rightAdjunct :: (a -> D b) -> C a -> b
  rightAdjunct f (C a) =
    let (D b) = f a
     in b
