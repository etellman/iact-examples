module Ch2.Sec3.Example31Test (tests) where

import Ch2.Sec3.Example31
import Data.Matrix
import Data.Monoid (All (All))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Monoid.BooleanMonoids (PartialOrdAll (PartialOrdAll))
import Preorder.Quantale
import qualified Properties.VCategoryProperties as VC
import Test.Tasty
import Test.Tasty.HUnit

genEx31 :: Gen Ex31
genEx31 = Gen.element $ fmap Ex31 ['p', 'q', 'r', 's', 't']

ex31hom :: Ex31 -> Ex31 -> PartialOrdAll
ex31hom x y = PartialOrdAll . All $ x <= y

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Example31Test"
    [ testCase "distance matrix" $ do
        let expected =
              fromLists $
                (fmap . fmap)
                  BoolWeight
                  [ [True, True, True, True, True],
                    [False, True, False, True, True],
                    [False, False, True, True, True],
                    [False, False, False, True, True],
                    [False, False, False, False, True]
                  ]
        distances ex31 @?= expected,
      testCase "p <= q" $ assertBool "<=" $ 'p' `lte31` 'q',
      testCase "p <= t" $ assertBool "<=" $ 'p' `lte31` 't',
      testCase "q > p" $ assertBool "<=" $ not ('q' `lte31` 'p'),
      VC.vCategoryTests "V-Category" genEx31 ex31hom
    ]
