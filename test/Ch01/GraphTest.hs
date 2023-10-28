module Ch01.GraphTest (tests) where

import Ch01.Graph
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Ch01.GraphTest"
    [ testCase "example 1.32" $ do
        -- set up
        let s 'a' = 1 :: Int
            s 'b' = 1
            s 'c' = 1
            s 'e' = 2
            s 'd' = 2
            s _ = undefined

            t 'a' = 2
            t 'b' = 3
            t 'c' = 3
            t 'd' = 2
            t 'e' = 3
            t _ = undefined

        let g = Graph [1, 2, 3] ['a' .. 'e'] s t

        -- exercise and verify
        compose g 'a' 'e' @?= 3
    ]
