module Ch01.PreorderTest (tests) where

import Ch01.Preorder
import Ch01.UpperSet
import Data.List (nub)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_product :: Property
prop_product = property $ do
  -- set up
  xs <- forAll $ Gen.list (Range.constant 1 5) (Gen.int $ Range.constant 0 1000)
  ys <- forAll $ Gen.list (Range.constant 1 5) Gen.alpha

  -- exercise
  let (Preorder h xys) = productPreorder (Preorder (<=) xs) (Preorder (<=) ys)

  -- verify
  (x1, y1) <- forAll $ Gen.element xys
  (x2, y2) <- forAll $ Gen.element xys

  (x1 <= x2 && y1 <= y2) === h (x1, y1) (x2, y2)

prop_opposite :: Property
prop_opposite = property $ do
  -- set up
  xs <- forAll $ nub <$> Gen.list (Range.constant 1 10) Gen.alpha
  let po = Preorder (<=) xs

  -- exercise
  let (Preorder oplte opelements) = oppositePreorder po

  -- verify
  x <- forAll $ Gen.element (xs)
  y <- forAll $ Gen.element (xs)

  (x <= y) === (oplte y x)
  opelements === xs

tests :: TestTree
tests =
  testGroup
    "Ch01.PreorderTest"
    [ testProperty "product" prop_product,
      testCase "exercise 1.52" $ do
        let xs = [1, 2] :: [Int]
            ys = ['a', 'b', 'c']
            po = productPreorder (Preorder lte ys) (Preorder (<=) xs)

            lte 'a' 'c' = True
            lte 'a' 'b' = True
            lte x y = x == y

        preorderConnections po
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
        let (Preorder _ usxs) = upperSetPreorder po
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
              ],
      testProperty "opposite" prop_opposite
    ]
