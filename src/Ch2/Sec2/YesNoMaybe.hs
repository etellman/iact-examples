module Ch2.Sec2.YesNoMaybe
  ( YesNoMaybe (..),
    YnmMin (..),
    YnmMax (..),
  )
where

import Preorder.Preorder

data YesNoMaybe = No | Maybe | Yes deriving (Eq, Ord, Show)

newtype YnmMin = YnmMin YesNoMaybe deriving (Eq, Ord, Show)

instance Preorder YnmMin where
  (<=) = (Prelude.<=)

instance Monoid YnmMin where
  mempty = YnmMin Yes

instance Semigroup YnmMin where
  (<>) = min

newtype YnmMax = YnmMax YesNoMaybe deriving (Eq, Ord, Show)

instance Preorder YnmMax where
  (<=) = (Prelude.<=)

instance Monoid YnmMax where
  mempty = YnmMax No

instance Semigroup YnmMax where
  (<>) = max
