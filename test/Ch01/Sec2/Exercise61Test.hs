module Ch01.Sec2.Exercise61Test (tests) where

import Ch01.MonotoneMap
import Ch01.Preorder
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

prop_part_1 :: Property
prop_part_1 = property $ do
  -- set up
  xss <- forAll $ nub <$> Gen.list (Range.constant 1 100) Gen.alpha
  p <- forAll $ Gen.element xss

  -- exercise
  let ap = arrow (<=) p xss

  -- verify
  H.assert $ isUpperSet (>=) ap xss

prop_part_2_and_3 :: (Show a, Eq a) => (a -> a -> Bool) -> Gen [a] -> Property
prop_part_2_and_3 lte generator = property $ do
  -- set up
  xs <- forAll $ nub <$> generator
  let po = Preorder lte xs

  -- exercise
  let arrowPreorder = arrowMonotoneMap po

  -- verify
  p <- forAll $ Gen.element xs
  q <- forAll $ Gen.element xs
  let arrow' x = arrow lte x xs

  isLte (opposite po) p q ==> isLte arrowPreorder (arrow' p) (arrow' q)
  (p `lte` q) === ((arrow' q) `isSubsetOf` (arrow' p))

tests :: TestTree
tests =
  testGroup
    "Ch01.Sec2.Exercise61Test"
    [ testProperty "part 1" prop_part_1,
      testProperty "parts 2 and 3" $
        prop_part_2_and_3 (<=) (Gen.list (Range.linear 1 20) Gen.alpha),
      testProperty "parts 4 with part 2 and 3 properties" $ do
        prop_part_2_and_3 lte61 (Gen.constant ['a' .. 'c']),
      testCase "part 4" $ do
        -- set up
        let xs = ['a' .. 'c']

        -- exercise
        let pairs = fmap (\x -> (x, arrow lte61 x xs)) xs
            po = Preorder lte61 xs
            upo = upperSetPreorder po

        -- exercise and verify
        pairs @?= [('a', "abc"), ('b', "b"), ('c', "c")]
        connections (opposite po) @?= [('b', 'a'), ('c', 'a')]
        connections upo
          @?= [ ("", "c"),
                ("", "b"),
                ("", "bc"),
                ("", "abc"),
                ("c", "bc"),
                ("c", "abc"),
                ("b", "bc"),
                ("b", "abc"),
                ("bc", "abc")
              ]
    ]
