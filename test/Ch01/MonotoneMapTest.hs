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
import TestLib.Labeled

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

prop_exercise_62 :: Property
prop_exercise_62 = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant (-10) 10)
  (Labeled _ f) <-
    forAll $ Gen.element [Labeled "+" (+ n), Labeled "*" (* n), Labeled "-" ((-) n)]
  (Labeled _ lte) <-
    forAll $ Gen.element [Labeled "<=" (<=), Labeled ">=" (>=), Labeled "==" (==)]

  let xs = [-10 .. 10]
      ys = [-100 .. 100]

  let dpo = P.Preorder (==) xs
      po = P.Preorder lte ys

  x <- forAll $ Gen.int (Range.constant (-10) 10)
  y <- forAll $ Gen.int (Range.constant (-10) 10)

  -- from the exercise:
  P.isLte dpo x y ==> P.isLte po (f x) (f y)

  -- since:
  (P.isLte dpo x y) === (x == y)

  -- this is just saying that every preorder is reflexive:
  H.assert $ P.isLte po (f x) (f x)

prop_exercise_64 :: Property
prop_exercise_64 = property $ do
  -- set up
  let xs = [1, 2, 3] :: [Int]
      ys = [2, 4, 6]
      f = (2 *)
      p = [[2], [4, 6]]
      q = [[2, 4], [6]]

      -- sp :: x -> p
      -- s . f ::
      sp 2 = 0
      sp 4 = 1
      sp 6 = 1


  let fstar p = sp p

  x <- forAll $ Gen.element xs

  f x === 2 * x


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

            -- exercise
            let pairs = fmap (\x -> (x, arrow lte61 x xs)) xs
                po = P.Preorder lte61 xs
                upo = upperSetPreorder po

            -- exercise and verify
            pairs @?= [('a', "abc"), ('b', "b"), ('c', "c")]
            P.connections (P.opposite po) @?= [('b', 'a'), ('c', 'a')]
            P.connections upo
              @?= [ ("", "c"),
                    ("", "b"),
                    ("", "bc"),
                    ("", "abc"),
                    ("c", "bc"),
                    ("c", "abc"),
                    ("b", "bc"),
                    ("b", "abc"),
                    ("bc", "abc")
                  ],
          testProperty "exercise 62" $ prop_exercise_62,
          testProperty "exercise 64" $ prop_exercise_64
        ]
    ]
