module Ch01.Sec1.Exercise37Test (tests) where

import Ch01.Set (cartesianProduct)
import Ch01.SetSystem
import qualified Data.PartialOrd as PO
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Ch01.Sec1.Exercise37Test" $ do
  -- set up
  let xs = partitions ['a', 'b', 'c']
      pairs = cartesianProduct xs xs

  -- verify
  length (filter (\(x, y) -> x PO.<= y) pairs) @=? 12
