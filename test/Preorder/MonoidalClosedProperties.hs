module Preorder.MonoidalClosedProperties (testClosed) where

import Data.Containers.ListUtils (nubOrd)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Preorder.MonoidalMapProperties
import Preorder.Preorder as PO
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Determines whether the two ways of finding the join are isomporphic
joinMatches ::
  (Monoid m, PO.Preorder m, MonadTest a) =>
  m ->
  Maybe m ->
  m ->
  a ()
joinMatches v j2 j1 = maybe failure (\j2' -> assert $ (v <> j1) =~ j2') j2

prop64b ::
  (Monoid m, Ord m, Show m, PO.Preorder m) =>
  Gen m ->
  ([m] -> Maybe m) ->
  Property
prop64b genElement preorderJoin = property $ do
  -- set up
  v <- forAll genElement
  as <- forAll $ nubOrd <$> Gen.list (Range.constant 0 10) genElement

  -- exercise
  let j1 = preorderJoin as
      j2 = preorderJoin $ fmap (v <>) as

  -- verify
  maybe failure (joinMatches v j2) j1

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
  (Monoid m, Ord m, Show m, PO.Preorder m) =>
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
