module Ch3.Sec4.RepresentableTest (tests) where

import Ch3.Sec4.AdjunctionExample
import Data.Char
import Data.Functor.Rep
import Data.Tuple
import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_tabulateSolo :: Property
prop_tabulateSolo = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise
  let tx = tabulate MkSolo

  -- verify
  tx x === MkSolo x

prop_indexSolo :: Property
prop_indexSolo = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise
  let ix = index MkSolo

  -- verify
  ix x === MkSolo x

prop_tabulateB :: Property
prop_tabulateB = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise
  let tx = tabulate B :: Int -> B Int

  -- verify
  tx x === B x

prop_indexB :: Property
prop_indexB = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constantBounded :: Range Int)

  -- exercise
  let ix = index B :: Int -> B Int

  -- verify
  ix x === B x

prop_tabulatePair :: Property
prop_tabulatePair = property $ do
  -- set up
  c <- forAll Gen.alpha

  -- exercise
  let tx = (tabulate ord) :: Char -> Int

  -- verify
  index tx c === ord c

tests :: TestTree
tests =
  testGroup
    "Ch3.Sec4.RepresentableTest"
    [ testGroup
        "B"
        [ testProperty "tabulate" prop_tabulateB,
          testProperty "index" prop_indexB
        ],
      testGroup
        "Solo"
        [ testProperty "tabulate" prop_tabulateSolo,
          testProperty "index" prop_indexSolo
        ],
      testProperty "ord" prop_tabulatePair
    ]
