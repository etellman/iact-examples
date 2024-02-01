module Ch1.Sec1.Exercise37Test (tests) where

import Ch1.Set (cartesianProduct)
import Ch1.SetSystem
import qualified Data.PartialOrd as PO
import Slist (slist)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Ch1.Sec1.Exercise37Test" $ do
  -- set up
  let xs = partitions ['a', 'b', 'c']
      pairs = cartesianProduct xs xs

  -- verify
  (length . slist) (filter (uncurry (PO.<=)) pairs) @=? 12
