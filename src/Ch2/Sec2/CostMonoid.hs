module Ch2.Sec2.CostMonoid
  ( ApproximateDouble,
    Cost (..),
    CostPreorder (..),
    CostOpPreorder (..),
  )
where

import Data.Eq.Approximate
import Lib.Preorder
import TypeLevel.NaturalNumber

type ApproximateDouble = AbsolutelyApproximateValue (Digits Five) Double

data Cost = Infinity | Cost ApproximateDouble deriving (Show, Eq)

instance Ord Cost where
  _ <= Infinity = True
  Infinity <= _ = False
  (Cost x) <= (Cost y) = x <= y

instance Monoid Cost where
  mempty = Cost 0

instance Semigroup Cost where
  Infinity <> _ = Infinity
  _ <> Infinity = Infinity
  (Cost x) <> (Cost y) = (Cost $ x + y)

newtype CostPreorder = CostPreorder Cost deriving (Show, Eq, Ord, Monoid, Semigroup)

instance Preorder CostPreorder where
  lte = (>=)

newtype CostOpPreorder = CostOpPreorder Cost deriving (Show, Eq, Ord, Monoid, Semigroup)

instance Preorder CostOpPreorder where
  lte = (<=)
