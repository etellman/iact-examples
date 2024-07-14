module Ch2.Sec3.Example31Test (tests) where

import Ch2.Sec3.Example31
import Data.Monoid (All (All))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Monoid.BooleanMonoids (PartialOrdAll (PartialOrdAll))
import qualified Properties.VCategoryProperties as VC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions ((==>))

genEx31 :: Gen Ex31
genEx31 = Gen.element $ fmap Ex31 ['p', 'q', 'r', 's', 't']

ex31hom :: Ex31 -> Ex31 -> PartialOrdAll
ex31hom x y = PartialOrdAll . All $ x <= y

prop_reflexive :: Property
prop_reflexive = property $ do
  -- set up
  x <- forAll genEx31
  y <- forAll genEx31

  cover 5 "same" $ x == y
  cover 20 "different" $ x /= y

  -- exercise and verify
  x <= y && y <= x ==> x == y

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Example31Test"
    [ VC.vCategoryTests "V-Category" genEx31 ex31hom,
      testCase "p <= q" $ assertBool "<=" $ Ex31 'p' <= Ex31 'q',
      testCase "p <= t" $ assertBool "<=" $ Ex31 'p' <= Ex31 't',
      testCase "q <= t" $ assertBool "<=" $ Ex31 'q' <= Ex31 't',
      testCase "q > p" $ assertBool "<=" $ Ex31 'q' > Ex31 'p',
      testProperty "reflexive" prop_reflexive
    ]
