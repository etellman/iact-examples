module Preorder.MatrixTest (tests) where

-- import Data.Matrix
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Preorder.MatrixTest"
    [ testCase "addition" $ 1 @?= (1 :: Int)
    ]
