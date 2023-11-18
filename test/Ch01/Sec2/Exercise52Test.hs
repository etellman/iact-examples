module Ch01.Sec2.Exercise52Test (tests) where

import qualified Ch01.Preorder as P
import Ch01.UpperSet
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Ch01.Sec2.Exercise52Test" $ do
  let xs = [1, 2] :: [Int]
      ys = ['a', 'b', 'c']
      po = P.productPreorder (P.Preorder lte ys) (P.Preorder (<=) xs)

      lte 'a' 'c' = True
      lte 'a' 'b' = True
      lte x y = x == y

  P.connections po
    @?= [ (('a', 1), ('a', 2)),
          (('a', 1), ('b', 1)),
          (('a', 1), ('b', 2)),
          (('a', 1), ('c', 1)),
          (('a', 1), ('c', 2)),
          (('a', 2), ('b', 2)),
          (('a', 2), ('c', 2)),
          (('b', 1), ('b', 2)),
          (('c', 1), ('c', 2))
        ]
  let (P.Preorder _ usxs) = upperSetPreorder po
  usxs
    @?= [ [],
          [('c', 2)],
          [('c', 1), ('c', 2)],
          [('b', 2)],
          [('b', 2), ('c', 2)],
          [('b', 2), ('c', 1), ('c', 2)],
          [('b', 1), ('b', 2)],
          [('b', 1), ('b', 2), ('c', 2)],
          [('b', 1), ('b', 2), ('c', 1), ('c', 2)],
          [('a', 2), ('b', 2), ('c', 2)],
          [('a', 2), ('b', 2), ('c', 1), ('c', 2)],
          [('a', 2), ('b', 1), ('b', 2), ('c', 2)],
          [('a', 2), ('b', 1), ('b', 2), ('c', 1), ('c', 2)],
          [('a', 1), ('a', 2), ('b', 1), ('b', 2), ('c', 1), ('c', 2)]
        ]
