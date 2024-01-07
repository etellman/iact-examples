module Ch2.Sec4.Example45Test (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Monoid.Cost (Cost (..))

f :: Cost -> Bool
f (Cost 0) = True
f _ = False

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec4.Example45Test"
    [
    ]
