{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monoid.BooleanMonoids
  ( PartialOrdAll (..),
    PartialOrdAny (..),
  )
where

import Data.Monoid (All, Any)
import Data.PartialOrd as PO

-- All which is also an instance of PartialOrd
newtype PartialOrdAll = PartialOrdAll All deriving (Show, Eq, Ord, Monoid, Semigroup)

instance PartialOrd PartialOrdAll where
  (<=) = (Prelude.<=)

-- Any which is also an instance of PartialOrd
newtype PartialOrdAny = PartialOrdAny Any deriving (Show, Eq, Ord, Monoid, Semigroup)

instance PartialOrd PartialOrdAny where
  (<=) = (Prelude.<=)
