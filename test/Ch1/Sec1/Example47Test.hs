module Ch1.Sec1.Example47Test (tests) where

import Ch1.SetSystem
import qualified Data.PartialOrd as PO
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Safe (atMay, headMay)
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

convertIndex :: Eq a => SetSystem a -> SetSystem a -> Maybe Int -> Maybe Int
convertIndex (SetSystem xs) ySystem i = i >>= atMay xs >>= headMay >>= labelFor ySystem

prop_example47 :: Property
prop_example47 = property $ do
  -- set up
  let systems = partitions ['a' .. 'e']
  system1 <- forAll $ Gen.element systems
  system2 <- forAll $ Gen.element systems

  c <- forAll $ Gen.element ['a' .. 'e']

  let p1 = labelFor system1
      p2 = labelFor system2
      p1To2 = convertIndex system1 system2

  -- exercise and verify
  system1 PO.<= system2 ==> (p1To2 . p1) c == p2 c

tests :: TestTree
tests = testProperty "Ch1.Sec1.Example47Test" prop_example47
