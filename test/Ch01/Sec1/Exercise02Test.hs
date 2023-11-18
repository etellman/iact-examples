module Ch01.Sec1.Exercise02Test (tests) where

import Ch01.Joinable
import Ch01.SetSystem
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Ch01.Sec1.Exercise02Test" $ do
  let s1 = SetSystem [[11, 12], [13], [21], [22, 23]] :: SetSystem Int
      s2 = SetSystem [[11], [21], [12, 22], [13, 23]]
  join s1 s2 @?= SetSystem [[11, 12, 13, 22, 23], [21]]
