module Ch01.MonotoneMapTest (tests) where

import Ch01.MonotoneMap
import qualified Ch01.Preorder as P
import Ch01.Set
import Ch01.UpperSet
import Data.List (nub)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions

-- comparison function for exercise 61 part 4
lte61 :: Char -> Char -> Bool
lte61 'a' 'b' = True
lte61 'a' 'c' = True
lte61 x y = x == y

prop_example55 :: Property
prop_example55 = property $ do
  -- set up
  x <- forAll $ Gen.bool
  y <- forAll $ Gen.bool
  let f False = 17 :: Int
      f True = 24

  -- exercise and verify
  x <= y ==> f x <= f y

prop_example57 :: Property
prop_example57 = property $ do
  -- set up
  xss <- forAll $ powerSet <$> nub <$> Gen.list (Range.constant 1 20) Gen.alpha

  xs <- forAll $ Gen.element xss
  ys <- forAll $ Gen.element xss

  -- exercise and verify
  (xs `isSubsetOf` ys) ==> (length xs <= length ys)

prop_exercise61_1 :: Property
prop_exercise61_1 = property $ do
  -- set up
  xss <- forAll $ nub <$> Gen.list (Range.constant 1 100) Gen.alpha
  p <- forAll $ Gen.element xss

  -- exercise
  let ap = arrow (<=) p xss

  -- verify
  H.assert $ isUpperSet (>=) ap xss

prop_exercise61_2_3 :: (Show a, Eq a) => (a -> a -> Bool) -> Gen [a] -> Property
prop_exercise61_2_3 lte generator = property $ do
  -- set up
  xs <- forAll $ nub <$> generator
  let po = P.Preorder lte xs

  -- exercise
  let arrowPreorder = arrowMonotoneMap po

  -- verify
  p <- forAll $ Gen.element xs
  q <- forAll $ Gen.element xs
  let arrow' x = arrow lte x xs

  P.isLte (P.opposite po) p q ==> P.isLte arrowPreorder (arrow' p) (arrow' q)
  (p `lte` q) === ((arrow' q) `isSubsetOf` (arrow' p))

tests :: TestTree
tests =
  testGroup
    "Ch01.MonotoneMapTest"
    [ testProperty "example 1.55" prop_example55,
      testProperty "example 1.57" prop_example57,
      testGroup
        "exercise 61"
        [ testProperty "part 1" prop_exercise61_1,
          testProperty "parts 2 and 3" $
            prop_exercise61_2_3 (<=) (Gen.list (Range.linear 1 20) Gen.alpha),
          testProperty "parts 4 with part 2 and 3 properties" $ do
            prop_exercise61_2_3 lte61 (Gen.constant ['a' .. 'c']),
          testCase "part 4" $ do
            -- set up
            let xs = ['a' .. 'c']
                pairs = fmap (\x -> (x, arrow lte61 x xs)) xs

            -- exercise and verify
            pairs
              @=? [ ('a', "abc"),
                    ('b', "b"),
                    ('c', "c")
                  ]
        ]
    ]
