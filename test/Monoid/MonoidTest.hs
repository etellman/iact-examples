module Monoid.MonoidTest (monoidTests) where

import Monoid.HomElementTest
import Test.Tasty

monoidTests :: TestTree
monoidTests =
  testGroup
    "Monoid.MonoidTest"
    [ Monoid.HomElementTest.tests
    ]
