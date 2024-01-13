module Preorder.MonoidalClosedProperties (testClosed) where

import Hedgehog as H
import Preorder.MonoidalMapProperties
import Preorder.Preorder as PO
import Test.Tasty

testClosed ::
  (Monoid m, Show m, PO.Preorder m) =>
  String ->
  Gen m ->
  (m -> m) ->
  (m -> m) ->
  TestTree
testClosed name gen leftAdjunct rightAdjunct =
  testGroup
    name
    [ namedLaxMonotoneMap "left" gen leftAdjunct,
      namedLaxMonotoneMap "right" gen rightAdjunct
    ]
