module Ch01.BooleanSystemTest (tests) where

import Ch01.BooleanSystem
import Ch01.Joinable
import Data.PartialOrd as PO
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch01.BooleanSystemTest"
    [ testCase "joinable" $ do
        let joinTest x y expected =
              join (BooleanSystem x) (BooleanSystem y) @=? BooleanSystem expected

        joinTest False False False
        joinTest False True True
        joinTest True False True
        joinTest True True True,
      testCase "partial order" $ do
        let ordTest x y expected =
              (BooleanSystem x) PO.<= (BooleanSystem y) @=? expected

        ordTest False False True
        ordTest False True True
        ordTest True False False
        ordTest True True True
    ]
