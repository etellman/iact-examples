module Preorder.MonoidalClosedProperties (testClosed) where

import Data.List (nub)
import TestLib.Assertions
import Data.Maybe (fromJust, isJust)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Preorder.MonoidalMapProperties
import Preorder.Preorder as PO
import Test.Tasty
import Test.Tasty.Hedgehog

prop64b ::
  (Monoid m, Eq m, Show m, PO.Preorder m) =>
  Gen m ->
  ([m] -> Maybe m) ->
  Property
prop64b gen join = property $ do
  -- set up
  v <- forAll gen
  xs <- forAll $ nub <$> Gen.list (Range.constant 0 10) gen

  -- exercise
  let j = join xs

  -- verify
  isJust j ==> (v <> (fromJust j)) =~ ((fromJust $ join $ fmap (v <>) xs))

prop64c ::
  (Monoid m, Show m, PO.Preorder m) =>
  Gen m ->
  (m -> m -> m) ->
  Property
prop64c gen (-*) = property $ do
  -- set up
  v <- forAll gen
  w <- forAll gen

  -- exercise and verify
  H.assert $ (v <> (v -* w)) PO.<= w

prop64d ::
  (Monoid m, Show m, PO.Preorder m) =>
  Gen m ->
  (m -> m -> m) ->
  Property
prop64d gen (-*) = property $ do
  -- set up
  v <- forAll gen

  -- exercise and verify
  H.assert $ v PO.=~ (mempty -* v)

prop64e ::
  (Monoid m, Show m, PO.Preorder m) =>
  Gen m ->
  (m -> m -> m) ->
  Property
prop64e gen (-*) = property $ do
  -- set up
  u <- forAll gen
  v <- forAll gen
  w <- forAll gen

  -- exercise and verify
  H.assert $ ((u -* v) <> (v -* w)) PO.<= (u -* w)

testClosed ::
  (Monoid m, Eq m, Show m, PO.Preorder m) =>
  String ->
  Gen m ->
  (m -> m -> m) ->
  ([m] -> Maybe m) ->
  m ->
  TestTree
testClosed name gen (-*) join v =
  let leftAdjunct = (v <>)
      rightAdjunct x = (x -* v)
   in testGroup
        name
        [ testGroup
            "lax monotone map"
            [ namedLaxMonotoneMap "left" gen leftAdjunct,
              namedLaxMonotoneMap "right" gen rightAdjunct
            ],
          testGroup
            "proposition 2.64"
            [ testProperty "b" $ prop64b gen join,
              testProperty "c" $ prop64c gen (-*),
              testProperty "d" $ prop64d gen (-*),
              testProperty "e" $ prop64e gen (-*)
            ]
        ]
