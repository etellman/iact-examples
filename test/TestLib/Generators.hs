module TestLib.Generators
  ( preorderElement,
  )
where

import Ch1.Preorder
import Hedgehog
import qualified Hedgehog.Gen as Gen

preorderElement :: Preorder a -> Gen a
preorderElement (Preorder _ xs) = Gen.element xs
