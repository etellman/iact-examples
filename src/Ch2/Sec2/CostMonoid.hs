module Ch2.Sec2.CostMonoid
  ( ApproximateDouble,
    Cost (..),
    CostPO (..),
  )
where

import Data.Eq.Approximate
import Lib.Preorder
import TypeLevel.NaturalNumber

type ApproximateDouble = AbsolutelyApproximateValue (Digits Five) Double

-- from p. 52
data Cost = Infinity | Cost ApproximateDouble deriving (Show, Eq)

instance Ord Cost where
  _ <= Infinity = True
  Infinity <= _ = False
  (Cost x) <= (Cost y) = x <= y

newtype CostPO = CostPO Cost deriving (Show, Eq, Ord)

instance Preorder CostPO where
  lte = (<=)

instance Monoid CostPO where
  mempty = CostPO $ Cost 0

instance Semigroup CostPO where
  (CostPO Infinity) <> _ = CostPO Infinity
  _ <> (CostPO Infinity) = CostPO Infinity
  (CostPO (Cost x)) <> (CostPO (Cost y)) = CostPO (Cost $ x + y)

-- -- from p. 52
-- data CostOp = Infinity | Cost ApproximateDouble deriving (Show, Eq, Ord)

-- instance Preorder Cost where
--   lte _ Infinity = True
--   lte Infinity _ = False
--   lte (Cost x) (Cost y) = x <= y

-- instance Monoid Cost where
--   mempty = Cost 0

-- instance Semigroup Cost where
--   Infinity <> _ = Infinity
--   _ <> Infinity = Infinity
--   (Cost x) <> (Cost y) = Cost (x + y)
