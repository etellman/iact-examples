module Ch01.Sec1.Exercise03Test (tests) where

import Ch01.Joinable
import Ch01.SetSystem
import qualified Data.PartialOrd as PO
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_exercise3 :: Property
prop_exercise3 = property $ do
  -- set up
  let systems = partitions ['a' .. 'e']
  system1 <- forAll $ Gen.element systems
  system2 <- forAll $ Gen.element systems

  -- exercise
  let joined = join system1 system2

  -- verify
  H.assert $ system1 PO.<= joined
  H.assert $ system2 PO.<= joined

  system3 <- forAll $ Gen.element systems
  system1 PO.<= system3 && system2 PO.<= system3 ==> joined PO.<= system3

tests :: TestTree
tests = testProperty "Ch01.Sec1.Exercise03Test" prop_exercise3
