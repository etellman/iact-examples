module Ch01.SetTest (tests) where

import Ch01.Set
import Test.Tasty
import Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Ch01.SetTest"
    [ testCase "power set" $ do
        -- set up
        let xs = ['a', 'b', 'c']

        -- exercise
        let xss = powerSet xs

        -- verify
        xss @?= ["abc", "ab", "ac", "a", "bc", "b", "c", ""]
    ]
