{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monoid.Cost
  ( Cost (..),
    CostPreorder (..),
    CostOpPreorder (..),
    (-*),
    IntCost (..),
  )
where

import Data.PartialOrd as PO

data Num a => Cost a = Infinity | Cost !a deriving Eq

instance (Show a, Num a) => Show (Cost a) where
  show (Cost x) = show x
  show Infinity = "∞"

newtype IntCost = IntCost (Cost Int) deriving (Show, Eq, Ord)

instance Semigroup IntCost where
  IntCost Infinity <> _ = IntCost Infinity
  _ <> IntCost Infinity = IntCost Infinity
  (IntCost (Cost x)) <> (IntCost (Cost y)) = IntCost $ Cost (x + y)

instance (Num a, Ord a) => Ord (Cost a) where
  _ <= Infinity = True
  Infinity <= _ = False
  (Cost x) <= (Cost y) = x Prelude.<= y

instance Num a => Monoid (Cost a) where
  mempty = Cost (fromInteger 0)

instance Num a => Semigroup (Cost a) where
  Infinity <> _ = Infinity
  _ <> Infinity = Infinity
  (Cost x) <> (Cost y) = Cost $ x + y

newtype Num a => CostPreorder a = CostPreorder (Cost a)
  deriving (Show, Eq, Ord, Monoid, Semigroup)

instance (Num a, Ord a) => PartialOrd (CostPreorder a) where
  (<=) = (Prelude.>=)

newtype Num a => CostOpPreorder a = CostOpPreorder (Cost a)
  deriving (Show, Eq, Ord, Monoid, Semigroup)

instance (Num a, Ord a) => PartialOrd (CostOpPreorder a) where
  (<=) = (Prelude.<=)

instance Num a => Num (Cost a) where
  (*) (Cost x) (Cost y) = Cost (x * y)
  (*) _ _ = Infinity

  (+) (Cost x) (Cost y) = Cost (x + y)
  (+) _ _ = Infinity

  abs = id
  signum _ = 1
  negate = id
  fromInteger x = Cost (fromInteger x)

-- | hom-element - see definition 2.57
(-*) :: (Num a, Ord a) => Cost a -> Cost a -> Cost a
(-*) (Cost x) (Cost y) = Cost $ max (fromInteger 0) (y - x)
(-*) Infinity Infinity = Cost 0
(-*) Infinity _ = Cost 0
(-*) _ Infinity = Infinity

infixr 6 -*
