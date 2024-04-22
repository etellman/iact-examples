{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monoid.Cost
  ( ApproximateDouble,
    Cost (..),
    CostPreorder (..),
    CostOpPreorder (..),
    (-*),
  )
where

import Data.Eq.Approximate
import Data.PartialOrd as PO
import TypeLevel.NaturalNumber

type ApproximateDouble = AbsolutelyApproximateValue (Digits Five) Double

data Cost = Infinity | Cost !ApproximateDouble deriving (Show, Eq)

instance Ord Cost where
  _ <= Infinity = True
  Infinity <= _ = False
  (Cost x) <= (Cost y) = x Prelude.<= y

instance Monoid Cost where
  mempty = Cost 0

instance Semigroup Cost where
  Infinity <> _ = Infinity
  _ <> Infinity = Infinity
  (Cost x) <> (Cost y) = Cost $ x + y

newtype CostPreorder = CostPreorder Cost deriving (Show, Eq, Ord, Monoid, Semigroup)

instance PartialOrd CostPreorder where
  (<=) = (Prelude.>=)

newtype CostOpPreorder = CostOpPreorder Cost deriving (Show, Eq, Ord, Monoid, Semigroup)

instance PartialOrd CostOpPreorder where
  (<=) = (Prelude.<=)

-- | hom-element - see definition 2.57
(-*) :: Cost -> Cost -> Cost
(-*) (Cost x) (Cost y) = Cost $ max 0 (y - x)
(-*) Infinity Infinity = Cost 0
(-*) Infinity _ = Cost 0
(-*) _ Infinity = Infinity

infixr 6 -*
