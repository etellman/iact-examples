module Preorder.MonoidalClosedProperties (testClosed) where

import Hedgehog as H
import Preorder.MonoidalMapProperties
import Preorder.Preorder as PO
import Test.Tasty
import Test.Tasty.Hedgehog

testC ::
  (Monoid m, Show m, PO.Preorder m) =>
  Gen m ->
  (m -> m -> m) ->
  Property
testC gen (-*) = property $ do
  -- set up
  v <- forAll gen
  w <- forAll gen

  -- exercise and verify
  H.assert $ (v <> (v -* w)) PO.<= w

testClosed ::
  (Monoid m, Show m, PO.Preorder m) =>
  String ->
  Gen m ->
  (m -> m -> m) ->
  m ->
  TestTree
testClosed name gen (-*) v =
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
            "proposition 2.61"
            [ testProperty "c" $ testC gen (-*)
            ]
        ]
