module Ch2.Sec5.Exercise59Test (tests) where

import Gen.Cost (genCost)
import Hedgehog as H
import Monoid.Cost
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

closedTest :: Cost -> Cost -> Cost -> Bool
closedTest v w a = (a <> v >= w) == (a >= v -* w)

prop_monoidalClosed :: Property
prop_monoidalClosed = property $ do
  -- set up
  v <- forAll $ genCost
  w <- forAll $ genCost
  a <- forAll $ genCost

  cover 10 "a <> v >= w" $ a <> v >= w
  cover 10 "a <> v < w" $ a <> v < w
  cover 5 "v == Infinity" $ v == Infinity
  cover 5 "w == Infinity" $ w == Infinity
  cover 5 "a == Infinity" $ a == Infinity

  -- exercise and verify
  H.assert $ closedTest v w a

-- mapFunction :: Cost -> Cost -> Cost
-- mapFunction v x = v -* x

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec5.Exercise59Test"
    [ testGroup
        "monoidal closed"
        [ testProperty "monoidal closed property" prop_monoidalClosed,
          testCase "infinity 0 0" $
            assertBool "infinity 0 0" $
              closedTest Infinity (Cost 0) (Cost 0)
        ]
    ]
