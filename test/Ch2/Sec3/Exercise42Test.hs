module Ch2.Sec3.Exercise42Test (tests) where

import Ch2.Sec3.Exercise42
import Data.Matrix
import Preorder.Quantale
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Ch2.Sec3.Exercise42Test" $ do
  let cities2 =
        fromLists $
          (fmap . fmap)
            TransportSet
            [ [[A, B, C], [A, C], [], [], []],
              [[], [A, B, C], [], [B, C], []],
              [[], [], [A, B, C], [A], []],
              [[], [], [], [A, B, C], [A, C]],
              [[A, B], [], [B], [C], [A, B, C]]
            ]
      expected =
        fromLists $
          (fmap . fmap)
            TransportSet
            [ [[A, B, C], [A, C], [], [C], [C]],
              [[], [A, B, C], [], [B, C], [C]],
              [[A], [A], [A, B, C], [A], [A]],
              [[A], [A], [], [A, B, C], [A, C]],
              [[A, B], [A], [B], [C], [A, B, C]]
            ]
  distances cities2 @?= expected
