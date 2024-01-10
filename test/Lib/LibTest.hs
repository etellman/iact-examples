module Lib.LibTest (libTests) where

import Preorder.PreorderTest
import Test.Tasty

libTests :: TestTree
libTests =
  testGroup
    "Lib.LibTest"
    [ Preorder.PreorderTest.tests
    ]
